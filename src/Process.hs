module Process where

import System.Exit
import System.IO
import System.Process


specProcess :: Maybe FilePath -> FilePath -> [String] -> CreateProcess
specProcess cwd cmd args =
  CreateProcess { cmdspec = RawCommand cmd args
                , cwd = cwd
                , env = Nothing
                , std_in = Inherit
                , std_out = Inherit
                , std_err = Inherit
                , close_fds = False
                , create_group = False
                , delegate_ctlc = False
                }

showProcess :: CreateProcess -> String
showProcess spec =
  let
    prefix = case cwd spec of
      Nothing -> "Running command"
      Just wd -> "Running command in " ++ showCommandForUser wd []
    postfix = case cmdspec spec of
      ShellCommand cmd -> cmd
      RawCommand cmd args -> showCommandForUser cmd args
  in
    prefix ++ ": " ++ postfix

evalProcess :: CreateProcess -> IO ()
evalProcess spec = do
  hPutStrLn stderr $ showProcess spec
  (Nothing, Nothing, Nothing, handle) <- createProcess spec
  exitCode <- waitForProcess handle
  case exitCode of
    ExitSuccess     -> return ()
    ExitFailure err -> die $ "Command failed with error code " ++ show err ++ "."

evalCommand :: Maybe FilePath -> FilePath -> [String] -> IO ()
evalCommand cwd cmd args = evalProcess $ specProcess cwd cmd args
