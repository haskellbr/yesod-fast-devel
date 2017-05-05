module Main where

import           Control.Concurrent           (forkIO, threadDelay)
import           Control.Concurrent.STM.TChan (TChan, dupTChan, newTChan,
                                               readTChan, writeTChan)
import           Control.Exception            (bracket)
import           Control.Monad                (forever, unless, when)
import           Control.Monad.STM            (atomically)
import qualified Data.ByteString.Lazy         as ByteString
import           Data.Digest.Pure.MD5         (md5)
import           Data.Maybe
import           Data.Monoid
import           Data.Text                    (isInfixOf, pack)
import           Options.Applicative
import           Paths_yesod_fast_devel
import           System.Console.ANSI
import           System.Directory             (copyFile, doesDirectoryExist,
                                               findExecutable)
import           System.Exit
import           System.FilePath              (takeDirectory)
import           System.FilePath.Glob
import           System.FilePath.Posix        (takeBaseName)
import           System.FSNotify              (Event (..), watchTree,
                                               withManager)
import           System.IO                    (BufferMode (..), Handle,
                                               hPutStrLn, hSetBuffering, stderr,
                                               stdout)
import           System.Process

data Options
  = PatchDevelMain { pdmFilePath :: FilePath}
  | PrintPatchedMain
  | StartServer { ssFilePath :: FilePath}

options :: ParserInfo Options
options =
  info
    (allCommands <**> helper)
    (header "Faster yesod-devel with GHCi and Browser Sync")
  where
    allCommands =
      subparser
        (patchDevelMain <> printPatchedMain <> startServer <>
         metavar "patch | server| print-patched-main")
    printPatchedMain =
      command
        "print-patched-main"
        (info (pure PrintPatchedMain) (progDesc "Print the patched DevelMain"))
    startServer =
      command
        "server"
        (info
           (StartServer . fromMaybe "app/DevelMain.hs" <$>
            optional (argument str (metavar "devel-main-path")) <**>
            helper)
           (progDesc "Start the development servers"))
    patchDevelMain =
      command
        "patch"
        (info
           (PatchDevelMain . fromMaybe "app/DevelMain.hs" <$>
            optional (argument str (metavar "devel-main-path")) <**>
            helper)
           (progDesc "Patch your devel main with browser-sync"))

main :: IO ()
main = do
  cmd <- execParser options
  case cmd of
    PatchDevelMain fp -> initYesodFastDevel fp
    StartServer fp -> go fp
    PrintPatchedMain ->
      putStrLn =<< readFile =<< getDataFileName "PatchedDevelMain.hs"
  where
    go develMainPth = do
      hSetBuffering stdout LineBuffering
      hSetBuffering stderr LineBuffering
      chan <- atomically newTChan
      _ <- forkIO $ do
          hPutStrLn stderr "Watching files for changes..."
          watchThread chan
      _ <- forkIO $ do
          hPutStrLn stderr "Spawning browser-sync..."
          browserSyncThread
      hPutStrLn stderr "Spawning GHCi..."
      _ <- replThread develMainPth chan
      return ()

initYesodFastDevel :: FilePath -> IO ()
initYesodFastDevel develMainPth = do
  verifyDirectory
  verifyDevelMain
  patchedDevelMain <- getDataFileName "PatchedDevelMain.hs"
  copyFile patchedDevelMain develMainPth
  putStrLn "Patched `DevelMain.hs`"
  browserSyncPth <- findExecutable "browser-sync"
  putStrLn "Make sure you have `foreign-store` on your cabal file"
  when (isNothing browserSyncPth) $
    putStrLn "Install `browser-sync` to have livereload at port 4000"
  exitSuccess
  where
    verifyDirectory = do
      let dir = takeDirectory develMainPth
      putStrLn ("Verifying `" ++ dir ++ "` exists")
      dexists <- doesDirectoryExist dir
      unless dexists $ do
        hPutStrLn stderr ("Directory `" ++ dir ++ "` not found")
        exitFailure
    verifyDevelMain = do
      putStrLn "Verifying `DevelMain.hs` isn't modified"
      userDevelMd5 <- md5 <$> ByteString.readFile develMainPth
      originalDevelMd5 <-
        md5 <$> (ByteString.readFile =<< getDataFileName "OriginalDevelMain.hs")
      patchedDevelMd5 <-
        md5 <$> (ByteString.readFile =<< getDataFileName "PatchedDevelMain.hs")
      when (userDevelMd5 == patchedDevelMd5) $ do
        putStrLn "DevelMain.hs is already patched"
        exitSuccess
      when (userDevelMd5 /= originalDevelMd5) $ do
        hPutStrLn stderr "Found a weird DevelMain.hs on your project"
        hPutStrLn stderr "Use `yesod-fast-devel print-patched-main`"
        exitFailure

browserSyncThread :: IO ()
browserSyncThread = do
  browserSyncPth <- findExecutable "browser-sync"
  when (isJust browserSyncPth) $ callCommand cmd
  where
    cmd =
      "browser-sync start --no-open --files=\"devel-main-since\" --proxy \"localhost:3000\" --port 4000"

watchThread :: TChan Event -> IO ()
watchThread writeChan =
  withManager $ \mgr
    -- start a watching job (in the background)
   -> do
    _ <- watchTree mgr "." shouldReload (reloadApplication writeChan)
    -- sleep forever (until interrupted)
    forever $ threadDelay 1000000000

replThread :: FilePath -> TChan Event -> IO ()
replThread develMainPth chan = do
  readChan <- atomically (dupTChan chan)
  bracket newRepl onError (onSuccess readChan)
  where
    onError (_, _, _, process) = do
      interruptProcessGroupOf process
      threadDelay 100000
      terminateProcess process
      threadDelay 100000
      waitForProcess process
    onSuccess readChan (Just replIn, _, _, _) = do
      hSetBuffering replIn LineBuffering
      threadDelay 1000000
      hPutStrLn replIn loadString
      hPutStrLn replIn startString
      forever $ do
        event <- atomically (readTChan readChan)
        putStrLn "-----------------------------"
        setSGR [SetColor Foreground Vivid Yellow]
        print event
        setSGR [Reset]
        putStrLn "-----------------------------"
        hPutStrLn replIn loadString
        hPutStrLn replIn startString
    onSuccess _ (_, _, _, _) = do
      hPutStrLn stderr "Can't open GHCi's stdin"
      exitFailure
    startString = "update"
    loadString = ":load " ++ develMainPth

shouldReload :: Event -> Bool
shouldReload event = not (or conditions)
  where
    fp =
      case event of
        Added filePath _    -> filePath
        Modified filePath _ -> filePath
        Removed filePath _  -> filePath
    conditions =
      [ notInPath ".git"
      , notInPath "yesod-devel"
      , notInPath "dist"
      , notInFile "#"
      , notInPath ".cabal-sandbox"
      , notInFile "flycheck_"
      , notInPath ".stack-work"
      , notInGlob (compile "**/*.sqlite3-*")
      , notInGlob (compile "*.sqlite3-*")
      , notInFile "stack.yaml"
      , notInGlob (compile "*.hi")
      , notInGlob (compile "**/*.hi")
      , notInGlob (compile "*.o")
      , notInGlob (compile "**/*.o")
      , notInFile "devel-main-since"
      ]
    notInPath t = t `isInfixOf` pack fp
    notInFile t = t `isInfixOf` pack (takeBaseName fp)
    notInGlob pt = match pt fp

reloadApplication :: TChan Event -> Event -> IO ()
reloadApplication chan event = atomically (writeTChan chan event)

newRepl :: IO (Maybe Handle, Maybe Handle, Maybe Handle, ProcessHandle)
newRepl =
  createProcess $ newProc "stack" ["ghci", "--ghc-options", "-O0 -fobject-code"]

newProc :: FilePath -> [String] -> CreateProcess
newProc cmd args =
  CreateProcess
  { cmdspec = RawCommand cmd args
  , cwd = Nothing
  , env = Nothing
  , std_in = CreatePipe
  , std_out = Inherit
  , std_err = Inherit
  , close_fds = False
  , create_group = True
  , delegate_ctlc = False
  }
