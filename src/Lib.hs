{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}

module Lib 
    ( AppConfig(..)  , TitleNum
    , initTitleNum   , checkPermissions
    , processContent , updateContent
    , isProcessed
    )
  where

import Data.Vector.Unboxed.Sized qualified as UV
import Data.List.NonEmpty        qualified as NE
import Data.ByteString           qualified as BS
import Data.Text                 qualified as T

import Text.Regex.Base.RegexLike (MatchText)
import Control.Monad.Reader      (MonadReader(..), Reader, runReader, asks)
import Control.Monad.State       (MonadState(..))
import Data.List.NonEmpty        (NonEmpty(..))
import Data.Text.Encoding        (decodeUtf8Lenient)
import System.Directory          (doesFileExist, getPermissions, readable, writable)
import Data.Traversable          (mapAccumL)
import Data.ByteString           (ByteString)
import Text.Regex.TDFA           (Regex, makeRegex, matchTest, matchOnceText)
import Control.Monad             ((>=>))
import Data.Foldable             (foldr')
import Data.Finite               (Finite, packFinite)
import Data.Either               (partitionEithers)
import Data.Maybe                (fromJust)
import Data.Array                (elems)
import Data.Bool                 (bool)
import Data.Text                 (Text)
import Data.Char                 (ord)

import System.IO

import Util 
    ( PrintRange , pattern RangeView , updateUpper , Vector
    , N7         , StartBase         , baseToNum   , inRange
    )

data AppConfig 
    = AppConfig
    { startBase      :: !StartBase
    , headingRng     :: !PrintRange
    , backupRequired :: !Bool
    }

data Chunk 
    = Code [Text]
    | Doc  [Text]
  deriving (Show)

infixr 5 +|

(+|) :: Text -> Chunk -> Chunk
x +| (Code xs) = Code $ x : xs
x +| (Doc xs)  = Doc $ x : xs

newtype TitleNum = TitleNum { unTitleNum :: Vector N7 }

succTitleNum :: MonadReader AppConfig m => Finite N7 -> TitleNum -> m TitleNum
succTitleNum idx tNum = do
    base <- asks $ baseToNum . startBase

    pure . TitleNum . UV.imap (setValByIdx base) $ unTitleNum tNum
  where
    setValByIdx b i v  
        | i < idx   = v
        | i > idx   = pred b
        | otherwise = succ v

getTitleNum :: PrintRange -> TitleNum -> Text
getTitleNum (RangeView l u) = (`T.snoc` '.') . T.intercalate "." . (`map` [l .. u]) . ((T.pack . show) .) . UV.index . unTitleNum

initTitleNum :: MonadReader AppConfig m => m TitleNum 
initTitleNum = asks (pred . baseToNum . startBase) >>= pure . TitleNum . UV.replicate

checkPermissions :: [FilePath] -> IO ([FilePath], [FilePath])
checkPermissions = (partitionEithers <$>) . traverse classify 
  where
    classify :: FilePath -> IO (Either FilePath FilePath)
    classify = fileExisted >=> either (pure . Left) havingProperPerms
      where
        boolToEither      = bool Left Right
        fileExisted       = liftA2 (liftA2 boolToEither) doesFileExist pure
        havingProperPerms = liftA2 (liftA2 boolToEither) checkPerms pure
        checkPerms        = (liftA2 (&&) readable writable <$>) . getPermissions

isProcessed :: FilePath -> IO Bool
isProcessed = getLastLine >=> pure . (signature `BS.isSuffixOf`)

signature :: ByteString
signature = "<!-- This line is appended by mdnumr. Do NOT remove or edit it, and do NOT work below it -->"

signatureText :: Text
signatureText = decodeUtf8Lenient signature

getLastLine :: FilePath -> IO ByteString
getLastLine file = withBinaryFile file ReadMode \h -> do
    size <- hFileSize h

    case size of
        0 -> pure BS.empty
        _ -> go h (fromIntegral size) False BS.empty
  where
    go :: Handle -> Int -> Bool -> ByteString -> IO ByteString
    go h !offset !noSuffixSpace !carry 
        | offset <= 0 = pure carry
        | otherwise   = do
            hSeek h AbsoluteSeek $ fromIntegral nextOffset

            bs <- BS.hGet h readSize

            let bsStripped        = bool (BS.dropWhileEnd isSpace) id noSuffixSpace $ bs
            let nextNoSuffixSpace = not (BS.null bsStripped) || noSuffixSpace

            case BS.elemIndexEnd lf bsStripped of
                Just i  -> pure $ BS.drop (succ i) bsStripped <> carry
                Nothing -> go h nextOffset nextNoSuffixSpace $ bsStripped <> carry
      where
        chunkSize  = 0x500
        readSize   = min chunkSize offset
        nextOffset = offset - readSize
        lf         = fromIntegral $ ord '\n'
        isSpace    = (`BS.elem` " \n\t\f\r\v")

parseChunk :: Text -> [Chunk]
parseChunk = NE.toList . snd . foldr' go (checkCodeEnd, Doc [] :| []) . T.lines . T.stripEnd
  where
    go :: Text -> (Text -> Bool, NonEmpty Chunk) -> (Text -> Bool, NonEmpty Chunk)
    go t (isDelim, yys@(y :| ys)) =
        case isDelim t of
            True 
                | Doc _ <- y  -> (checkCodeBegin, Code [t] `NE.cons` yys)
                | Code _ <- y -> (checkCodeEnd, Doc [] :| (t +| y) : ys)
            False -> 
                (isDelim, (t +| y) :| ys)

    checkCodeBegin :: Text -> Bool
    checkCodeBegin = matchTest codeBeginPattern

    checkCodeEnd :: Text -> Bool
    checkCodeEnd = matchTest codeEndPattern

    codeBeginPattern :: Regex
    codeBeginPattern = makeRegex ("^ {0,3}```.*$" :: Text)

    codeEndPattern :: Regex
    codeEndPattern = makeRegex ("^ {0,3}```[ \\t\\r\\v\\f]*$" :: Text)

assembleChunk :: [Chunk] -> Text
assembleChunk =  T.unlines . concat . map \case
    Code x -> x
    Doc  x -> x

processContent :: (MonadReader AppConfig m, MonadState TitleNum m) => Text -> m Text
processContent = traverse numberChunk . parseChunk >=> pure . assembleChunk . (<> [Doc [signatureText]])

updateContent :: (MonadReader AppConfig m, MonadState TitleNum m) => Text -> m Text
updateContent = traverse numberChunk . map unnumberChunk . parseChunk >=> pure . assembleChunk
  where
    unnumberChunk :: Chunk -> Chunk
    unnumberChunk (Doc cont)    = Doc $ rmvTitleNum cont
    unnumberChunk code@(Code _) = code

numberChunk :: (MonadReader AppConfig m, MonadState TitleNum m) => Chunk -> m Chunk
numberChunk (Doc content) = Doc <$> addTitleNum content
numberChunk code@(Code _) = pure code

addTitleNum :: (MonadReader AppConfig m, MonadState TitleNum m) => [Text] -> m [Text]
addTitleNum content = do
    appCfg <- ask
    curNum <- get

    let (curNum', result) = mapAccumL (addTitleNum' appCfg) curNum content

    put curNum'
    pure result
  where
    addTitleNum' :: AppConfig -> TitleNum -> Text -> (TitleNum, Text)
    addTitleNum' cfg@(AppConfig { headingRng }) num line =
        case getTitle line of
            Just [pre, hash, gap, title] 
                | not $ inRange headingRng titleLevel -> (num, line)
                | otherwise -> 
                    let
                        numToApply   = runWithCfg $ succTitleNum titleLevel num
                        rangeToPrint = fromJust $ updateUpper titleLevel headingRng
                    in
                        (numToApply, T.concat [pre, hash, gap, getTitleNum rangeToPrint numToApply, " ",  title])
              where
                titleLevel = fromJust . packFinite . fromIntegral . T.length $ hash
            Just _  -> undefined
            Nothing -> (num, line)
      where
        runWithCfg :: Reader AppConfig a -> a 
        runWithCfg = flip runReader cfg

    getTitle :: Text -> Maybe [Text]
    getTitle = extractGroupFromReResult . matchOnceText titlePattern

    titlePattern :: Regex
    titlePattern = makeRegex ("(^ {0,3})(#{1,6})( +)(.+)$" :: Text)

rmvTitleNum :: [Text] -> [Text]
rmvTitleNum = map stripNumbering
  where
    stripNumbering :: Text -> Text
    stripNumbering line = 
        case extractGroupFromReResult $ matchOnceText titlePattern line of
            Just [pre, hash, _, gap, title] -> T.concat [pre, hash, gap, title]
            Just _                          -> undefined
            Nothing                         -> line

    titlePattern :: Regex
    titlePattern = makeRegex ("^( {0,3})(#{1,6}) +([0-9]+\\.){1,6}( +)(.+)$" :: Text)

extractGroupFromReResult :: Maybe (a, MatchText a, a) -> Maybe [a]
extractGroupFromReResult (Just (_, match, _)) = Just . map fst . drop 1 . elems $ match
extractGroupFromReResult Nothing              = Nothing

