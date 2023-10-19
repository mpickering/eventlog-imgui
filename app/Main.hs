{-# language BlockArguments     #-}
{-# language LambdaCase         #-}
{-# language OverloadedStrings  #-}
{-# LANGUAGE ApplicativeDo      #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE RankNTypes         #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE BangPatterns    #-}

module Main ( main ) where

import Control.Applicative
import Control.Exception
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Managed
import Data.Maybe
import DearImGui
import DearImGui.OpenGL2
import DearImGui.SDL
import DearImGui.SDL.OpenGL
import DearImGui.Plot hiding (plotLine, setupAxisLimits)
import DearImGui.Raw.Plot
import Graphics.GL
import SDL hiding (Event, Error, Timestamp)

import Control.Concurrent.STM    (STM, atomically, readTVar)
import Data.Foldable             (for_)
import Data.Void                 (Void)
import Data.Word                 (Word64)
import System.IO                 (Handle)
import Text.Printf               (PrintfArg, PrintfType, printf)

import qualified Data.ByteString          as BS
import qualified Data.IntSet              as IS
import qualified Data.Map.Strict          as Map
import qualified Data.Text                as T
import qualified Network.Socket           as Sock
import qualified Options.Applicative      as O
import qualified System.IO                as IO
--import qualified System.Remote.Monitoring

import GHC.Eventlog.Machines
import GHC.RTS.Events

import Data.Machine        (MachineT, ProcessT, await, repeatedly, runT_, (~>), yield, stop, runT, construct, taking, (<~))
import Data.Machine.Fanout (fanout)

import Foreign.C.Types (CFloat)
import System.Random (randomRIO)
import Data.Function
import GHC.RTS.Events.Incremental
import Control.Concurrent.Chan
import Control.Concurrent.STM.TChan
import Control.Concurrent hiding (yield)
import Foreign (withArrayLen)
import Data.Text.Foreign
import Data.IORef (newIORef)

newtype VisEnv =
    VisEnv
      { visEnvLiveBytesChan :: TChan EventEvent
      }

data VisState =
    VisState
      { visStateLiveBytes :: TimeSeries
      , visStateBlocksSize  :: TimeSeries
      , visStateHeapSize :: TimeSeries
      }

data TimeSeries =
    TimeSeries
      { timeSeriesTimes :: [CFloat]
      , timeSeriesVals :: [CFloat]
      }

emptyTimeSeries = TimeSeries [] []


-- 'Void' output to help inference.
fileSink :: Handle -> ProcessT IO (Maybe BS.ByteString) Void
fileSink hdl = repeatedly $ do
    mbs <- await
    liftIO $ for_ mbs $ BS.hPut hdl

liveBytesM :: ProcessT IO Event EventEvent
liveBytesM = construct $ fix $ \r -> do
  Event t ei _ <- await <|> stop
  case ei of
    HeapLive _ lb -> yield (H (t, lb)) >> r
    BlocksSize _ lb -> yield (B (t, lb)) >> r
    HeapSize _ lb -> yield (M (t, lb)) >> r

    _ -> r

data EventEvent = H (Word64, Word64) | B (Word64, Word64) | M (Word64, Word64) deriving (Show)

-------------------------------------------------------------------------------
-- connecting & opening
-------------------------------------------------------------------------------

connectToEventlogSocket :: FilePath -> IO Handle
connectToEventlogSocket socketName = do
    s <- Sock.socket Sock.AF_UNIX Sock.Stream Sock.defaultProtocol
    Sock.connect s (Sock.SockAddrUnix socketName)
    h <- Sock.socketToHandle s IO.ReadMode
    IO.hSetBuffering h IO.NoBuffering
    return h

openEventlogFile :: FilePath -> IO Handle
openEventlogFile path = do
    hdl <- IO.openFile path IO.ReadMode
    IO.hSetBinaryMode hdl True
    return hdl

-------------------------------------------------------------------------------
-- Options
-------------------------------------------------------------------------------

data PathType = PathTypeSocket | PathTypeFile
  deriving Show

data Opts = Opts
    { optEventLogPath :: FilePath
    , optPathType     :: PathType
    }
  deriving Show

optsP :: O.Parser Opts
optsP = do
    optEventLogPath <- O.strArgument $
        O.metavar "PATH" <> O.help "Eventlog path"
    optPathType <- pathTypeP

    pure Opts {..}
  where
    pathTypeP :: O.Parser PathType
    pathTypeP =
        O.flag' PathTypeSocket (O.long "unix" <> O.help "Path is to UNIX socket (default)") <|>
        O.flag' PathTypeFile (O.long "file" <> O.help "Path is to ordinary file") <|>
        pure PathTypeSocket

printC = repeatedly (await >>= \x -> liftIO (print x) >> yield x)

main :: IO ()
main = do
    opts <- O.execParser $ O.info (optsP <**> O.helper) $
        O.fullDesc <> O.header "eventlog-imgui"

    hdl <- case optPathType opts of
        PathTypeSocket -> connectToEventlogSocket (optEventLogPath opts)
        PathTypeFile   -> openEventlogFile (optEventLogPath opts)

    let input :: MachineT IO k (Maybe BS.ByteString)
        input = sourceHandleWait hdl 1_000_000 4096

    let events :: ProcessT IO (Maybe BS.ByteString) Event
        events = decodeEventsMaybe
            ~> reorderEvents 1_000_000_000
            ~> checkOrder (curry print)

    event_chan <- newTChanIO

    forkIO $ runT_ (input ~> events ~> printC ~> liveBytesM ~> sinkChan event_chan)

    mainWindow event_chan

mainWindow :: TChan EventEvent -> IO ()
mainWindow inp = do
  -- Initialize SDL
  initializeAll

  runManaged do
    -- Create a window using SDL. As we're using OpenGL, we need to enable OpenGL too.
    window <- do
      let title = "Hello, Dear ImGui!"
      let config = defaultWindow { windowGraphicsContext = OpenGLContext defaultOpenGL }
      managed $ bracket (createWindow title config) destroyWindow

    -- Create an OpenGL context
    glContext <- managed $ bracket (glCreateContext window) glDeleteContext

    -- Create an ImGui context
    _ <- managed $ bracket createContext destroyContext
    _ <- managed $ bracket createPlotContext destroyPlotContext

    -- Initialize ImGui's SDL2 backend
    _ <- managed_ $ bracket_ (sdl2InitForOpenGL window glContext) sdl2Shutdown

    -- Initialize ImGui's OpenGL backend
    _ <- managed_ $ bracket_ openGL2Init openGL2Shutdown

    let
      initVisEnv = VisEnv inp
      initVisState =
        VisState
          { visStateLiveBytes = emptyTimeSeries
          , visStateBlocksSize = emptyTimeSeries
          , visStateHeapSize = emptyTimeSeries
          }

    liftIO $ mainLoop initVisEnv initVisState window

sinkChan chan  = repeatedly $ do
  await >>= \e -> do
    liftIO $ print e
    liftIO $ atomically (writeTChan chan e)

updateTimeSeries ts (t, v) =
          let
            new_t = fromIntegral t
            new_v = fromIntegral v
          in
            TimeSeries
              { timeSeriesTimes = new_t : timeSeriesTimes ts
              , timeSeriesVals = new_v : timeSeriesVals ts
              }

mainLoop :: VisEnv -> VisState -> Window -> IO ()
mainLoop env@VisEnv{..} state window = unlessQuit do
  -- Tell ImGui we're starting a new frame
  openGL2NewFrame
  sdl2NewFrame
  newFrame

  new_live_bytes <- atomically (tryReadTChan visEnvLiveBytesChan)
  let
    !new_state =
      case new_live_bytes of
        Nothing ->
          state
        Just (H tv) ->
          state { visStateLiveBytes = updateTimeSeries (visStateLiveBytes state) tv }
        Just (B tv) ->
          state { visStateBlocksSize = updateTimeSeries (visStateBlocksSize state) tv }
        Just (M tv) ->
          state { visStateHeapSize = updateTimeSeries (visStateHeapSize state) tv }


  -- Render
  glClear GL_COLOR_BUFFER_BIT

  pos <- newIORef (ImVec2 50 50)
  centered <- newIORef (ImVec2 0.5 0.5)
  size <- newIORef (ImVec2 600 750)

  setNextWindowPos pos ImGuiCond_Appearing Nothing
  setNextWindowSize size ImGuiCond_Appearing

  let series label ts =
          withArrayLen (timeSeriesTimes ts) $
            \l ts_ptr -> withArrayLen (timeSeriesVals ts) $
              \_ vs_ptr -> withCString label $
                \label -> plotLine label ts_ptr vs_ptr (fromIntegral l)

  bracket_ (begin "Heap usage") end $ do
    when (isJust new_live_bytes) setNextAxesToFit
    do
      success <- beginPlot "Live bytes"
      when success $ do
        series "Live Bytes" (visStateLiveBytes new_state)
        series "Blocks Size" (visStateBlocksSize new_state)
        series "Heap Size" (visStateHeapSize new_state)

        endPlot

  render
  openGL2RenderDrawData =<< getDrawData

  glSwapWindow window

  mainLoop env new_state window

  where
    -- Process the event loop
    unlessQuit action = do
      shouldQuit <- checkEvents
      unless shouldQuit action

    checkEvents = do
      pollEventWithImGui >>= \case
        Nothing ->
          return False
        Just event ->
          (isQuit event ||) <$> checkEvents

    isQuit event =
      SDL.eventPayload event == SDL.QuitEvent

