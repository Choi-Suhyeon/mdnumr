{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}

module Lib where

import Data.Vector.Unboxed.Sized qualified as UV
import Data.ByteString           qualified as BS
import Data.Text                 qualified as T

import Control.Monad.Reader (MonadReader(..), asks)
import System.Directory     (doesFileExist, getPermissions, readable, writable)
import System.IO.Error      (catchIOError)
import Data.ByteString      (ByteString)
import Text.Regex.TDFA      ((=~))
import Control.Monad        ((>=>))
import Data.Finite          (Finite, packFinite)
import Data.Either          (partitionEithers)
import Data.Monoid          (First(..))
import Data.Maybe           (isNothing)
import Data.Word            (Word8)
import Data.Bool            (bool)
import Data.Text            (Text)
import Data.Char            (ord)

import System.IO

import Util (HeadingRange, PrintRange, pattern RangeView, Vector, N7, StartBase, baseToNum)

data AppConfig 
    = AppConfig
    { startBase      :: !StartBase
    , headingRng     :: !HeadingRange
    , backupRequired :: !Bool
    }

newtype TitleNum = TitleNum { unTitleNum :: Vector N7 }

succTitleNum :: MonadReader AppConfig m => Finite N7 -> TitleNum -> m TitleNum
succTitleNum idx tNum = do
    base <- asks $ baseToNum . startBase

    pure . TitleNum $ (`UV.imap` unTitleNum tNum) \i e -> case compare i idx of
        EQ -> succ e
        GT -> pred base
        LT -> e

getTitleNum :: PrintRange N7 -> TitleNum -> Text
getTitleNum (RangeView l u) = T.pack . concat . (`map` [l .. u]) . (((++ ".") . show) .) . UV.index . unTitleNum

initialTitleNum :: MonadReader AppConfig m => m TitleNum 
initialTitleNum = asks (pred . baseToNum . startBase) >>= pure . TitleNum . UV.replicate

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

signature :: ByteString
signature = "<!-- This line is appended by mdnumr. Do NOT remove or edit it, and do NOT work below it -->"

isSignature :: ByteString -> Bool
isSignature = (signature `BS.isSuffixOf`)

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

{- Todo : mapAccumL을 사용해볼 것.
addTitleNum :: MonadReader AppConfig m => Text -> m Text
addTitleNum text = do
    
  where
    preprocessed = T.lines . T.stripEnd $ text

    addTitleNum' :: MonadReader AppConfig m => TitleNum -> [Text] -> m Text
    addTitleNum' 
-}

rmvTitleNum :: Text -> Text
rmvTitleNum = T.unlines . map stripNumbering . dropLast . T.lines . T.stripEnd
  where
    dropLast :: [a] -> [a]
    dropLast [] = []
    dropLast xs = init xs

    stripNumbering :: Text -> Text
    stripNumbering line = case (line =~ titlePattern :: (Text, Text, Text, [Text])) of 
        (_, _, _, g@[_, _, _, _]) -> T.concat g
        _                         -> line

    titlePattern :: Text
    titlePattern = "^( {0,3})(#{1,6})(?: +(?:[0-9]+\\.){1,6})( +)(.+)$"

