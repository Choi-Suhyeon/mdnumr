{-# LANGUAGE BlockArguments #-}

module CliOptions (CmdLineArgs(..), opts) where

import Options.Applicative 
    ( ParserInfo      , Parser  , ReadM
    , argument        , switch  , option     , help 
    , showDefaultWith , short   , long       , str
    , eitherReader    , metavar , value      , info
    , parserFailure   , header  , fullDesc   , progDesc
    , renderFailure   , helper  , infoOption , hidden
    , simpleVersioner , flag
    )
import Options.Applicative.Builder (ParseError(..), prefs, columns)
import Control.Applicative         ((<**>), some)
import Data.Either.Extra           (maybeToEither)
import Text.Regex.TDFA             ((=~))
import Data.Function               (on)
import GHC.TypeLits                (KnownNat)
import Data.Version                (showVersion)
import Data.Finite                 (Finite, packFinite)
import Data.Maybe                  (fromJust)

import Paths_mdnumr (version)

import Util (StartBase(..), PrintRange, mkRange)


data CmdLineArgs
    = CmdLineArgs 
    { fileNames     :: [FilePath]
    , base          :: StartBase 
    , backupEnabled :: Bool
    , range         :: PrintRange
    }

progName :: String
progName = "mdnumr"

opts :: ParserInfo CmdLineArgs
opts = info (pCmdLineArgs <**> helper <**> manOpt <**> versionOpt)
    $  fullDesc
    <> progDesc "Adds numbering to markdown headers, with optional backups."
    <> header   (progName ++ " - Markdown header numbering tool")

pCmdLineArgs :: Parser CmdLineArgs
pCmdLineArgs = CmdLineArgs <$> pArgs <*> pZeroBased <*> pBackupEnabled <*> pRange
  where
    pArgs = some $ argument str $ metavar "FILES"

    pZeroBased = flag OneBased ZeroBased
        $  short 'z'
        <> long "set-zero"
        <> help  "Start numbering from zero"

    pBackupEnabled = switch 
        $  short 'b' 
        <> long  "backup" 
        <> help  "Enable backup of original files with a '.bak' extension"

    pRange = option rangeReader
        $  short           'r'
        <> long            "range"
        <> help            "Specify the range of header levels to number"
        <> metavar         "N-M"
        <> value           (fromJust $ on mkRange partialPackFinite 1 6)
        <> showDefaultWith (const "1-6")

    rangeReader :: ReadM PrintRange
    rangeReader = eitherReader \s ->
        case s =~ "^([1-6])-([1-6])$" :: [[String]] of
            [[_, a, b]] -> maybeToEither rangeErrMsg $ (mkRange `on` (partialPackFinite . read)) a b
            _           -> Left formatErrMsg
      where
        rangeErrMsg  = "Invalid format: N-M with 1 ≤ N ≤ M ≤ 6"
        formatErrMsg = "Invalid format: must be N-M with digits 1–6"

    partialPackFinite :: KnownNat n => Integer -> Finite n
    partialPackFinite = fromJust . packFinite

manOpt :: Parser (a -> a)
manOpt = infoOption (helpAndExtra opts)
    $  hidden
    <> long  "man"
    <> help  "Show extended help"
  where
    helpAndExtra :: ParserInfo a -> String
    helpAndExtra pInfo = helpText <> "\n\n" <> manText
      where
        failure  = parserFailure (prefs $ columns 80) pInfo (ShowHelpText Nothing) mempty
        helpText = fst $ renderFailure failure progName

    manText :: String
    manText = unlines
      [ "Behavior:"
      , "  - --backup creates a '.bak' copy of each processed file."
      , "  - Code blocks are left untouched."
      , "  - A marker is appended to avoid re-processing already-numbered files."
      , ""
      , "Examples:"
      , "  $ " <> progName <> " README.md -r 2-4"
      , "  $ " <> progName <> " docs/*.md --backup"
      ]

versionOpt :: Parser (a -> a)
versionOpt = simpleVersioner $ showVersion version

