{-# language BlockArguments #-}
{-# language LambdaCase #-}
{-# language OverloadedStrings #-}
{-# LANGUAGE ApplicativeDo      #-}
{-# LANGUAGE BangPatterns       #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RankNTypes         #-}
{-# LANGUAGE RecordWildCards    #-}

module Main ( main ) where

import Control.Exception
import Control.Monad.IO.Class
import Control.Monad.Managed
import DearImGui
import DearImGui.OpenGL2
import DearImGui.SDL
import DearImGui.SDL.OpenGL
import Graphics.GL
import SDL hiding (Event)

import Control.Applicative       (optional, (<**>), (<|>))
import Control.Concurrent.STM    (STM, atomically, readTVar)
import Control.Monad.IO.Class    (liftIO)
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

import Data.Machine        (MachineT, ProcessT, await, repeatedly, runT_, (~>))
import Data.Machine.Fanout (fanout)

import Foreign.C.Types (CFloat)
import System.Random (randomRIO)

-- 'Void' output to help inference.
fileSink :: Handle -> ProcessT IO (Maybe BS.ByteString) Void
fileSink hdl = repeatedly $ do
    mbs <- await
    liftIO $ for_ mbs $ BS.hPut hdl

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
            ~> checkOrder (\e e' -> print (e, e'))

    mainWindow

mainWindow :: IO ()
mainWindow = do
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

    -- Initialize ImGui's SDL2 backend
    _ <- managed_ $ bracket_ (sdl2InitForOpenGL window glContext) sdl2Shutdown

    -- Initialize ImGui's OpenGL backend
    _ <- managed_ $ bracket_ openGL2Init openGL2Shutdown

    liftIO $ mainLoop [] window


mainLoop :: [CFloat] -> Window -> IO ()
mainLoop nums window = unlessQuit do
  -- Tell ImGui we're starting a new frame
  openGL2NewFrame
  sdl2NewFrame
  newFrame

  -- Build the GUI
  withWindowOpen "Hello, ImGui!" do
    -- Add a text widget
    text "Hello, ImGui!"

    -- Add a button widget, and call 'putStrLn' when it's clicked
    button "Clickety Click" >>= \case
      False -> return ()
      True  -> putStrLn "Ow!"

  -- Show the ImGui demo window
  showDemoWindow


  plotLines "" nums

  -- Render
  glClear GL_COLOR_BUFFER_BIT

  render
  openGL2RenderDrawData =<< getDrawData

  glSwapWindow window

  num <- randomRIO (-10.0, 10.0)
  mainLoop (num:nums) window

  where
    -- Process the event loop
    unlessQuit action = do
      shouldQuit <- checkEvents
      if shouldQuit then pure () else action

    checkEvents = do
      pollEventWithImGui >>= \case
        Nothing ->
          return False
        Just event ->
          (isQuit event ||) <$> checkEvents

    isQuit event =
      SDL.eventPayload event == SDL.QuitEvent
