{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE LambdaCase #-}

module Main (main) where

import Data.Text.IO qualified as TIO

import Control.Monad.Trans.Except (ExceptT(..), runExceptT)
import Control.Monad.State.Strict (State, evalState)
import Data.Functor.Identity      (Identity(..))
import Control.Monad.Reader       (ReaderT, runReaderT)
import Options.Applicative        (execParser)
import Control.Exception          (try, IOException)
import System.Directory           (copyFile)
import Data.Traversable           (for)
import System.FilePath            (takeDirectory, takeFileName, (</>))
import Data.Foldable              (traverse_)
import Control.Monad              (when)
import Data.Either                (isLeft)
import Data.Maybe                 (catMaybes)
import Data.Text                  (Text)
import Data.Bool                  (bool)
import System.IO                  (hPutStrLn, stderr)

import CliOptions (CmdLineArgs(..), opts)
import Lib 
    ( AppConfig(..) , TitleNum
    , isProcessed   , checkPermissions
    , updateContent , processContent
    , initTitleNum
    )

type MdNumberingM = ReaderT AppConfig (State TitleNum) Text

main :: IO ()
main = main' =<< execParser opts

main' :: CmdLineArgs -> IO ()
main' args = do
    let appCfg = cmdLineArgsToAppConfig args

    (inaccessibles, accessibles) <- checkPermissions $ fileNames args

    failed <- fmap catMaybes $ for accessibles \fileName -> do
        isLeft <$> runExceptT (processOneFile appCfg fileName) >>= \case
            True  -> pure $ Just fileName
            False -> pure Nothing

    dumpErrs "File Unavailable" inaccessibles
    dumpErrs "Processing Error" failed

cmdLineArgsToAppConfig :: CmdLineArgs -> AppConfig
cmdLineArgsToAppConfig (CmdLineArgs { base, backupEnabled, range })
    = AppConfig 
    { startBase      = base
    , headingRng     = range
    , backupRequired = backupEnabled 
    }

dumpErrs :: (Foldable t, Functor t) => String -> t String -> IO ()
dumpErrs summary = traverse_ (hPutStrLn stderr) . fmap ("[E] " <> summary <> ": " ++)

processOneFile :: AppConfig -> FilePath -> ExceptT IOException IO ()
processOneFile appCfg fileName = do
    processed <- liftTry $ isProcessed fileName
    content   <- liftTry $ TIO.readFile fileName

    liftTry $ when (backupRequired appCfg) (copyFile fileName $ makeHiddenBackupPath fileName)

    let initNum = runIdentity $ runReaderT initTitleNum appCfg
    let result  = evalMdNumberingM appCfg initNum . bool processContent updateContent processed $ content

    liftTry $ TIO.writeFile fileName result
  where
    makeHiddenBackupPath :: FilePath -> FilePath
    makeHiddenBackupPath = (</>) <$> takeDirectory <*> makeFileName . takeFileName
      where
        makeFileName []          = []
        makeFileName ccs@('.':_) = ccs ++ ".bak"
        makeFileName ccs         = '.' : ccs ++ ".bak"

liftTry :: IO a -> ExceptT IOException IO a
liftTry = ExceptT . try

evalMdNumberingM :: AppConfig -> TitleNum -> MdNumberingM -> Text
evalMdNumberingM env st m = flip evalState st $ runReaderT m env


