{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Course.FastAnagrams where

import Course.Core
import Course.List
import Course.Functor
import qualified Data.Set as S

-- Return all anagrams of the given string
-- that appear in the given dictionary file.
fastAnagrams ::
  Chars
  -> Filename
  -> IO (List Chars)
fastAnagrams str file = 
  let 
      strSet :: S.Set NoCaseString
      strSet = S.fromList . hlist . map NoCaseString . permutations $ str
      inSet a = S.member a strSet
  in (map ncString . filter inSet . map NoCaseString . lines) <$> readFile file

newtype NoCaseString =
  NoCaseString {
    ncString :: Chars
  }

instance Eq NoCaseString where
  (==) = (==) `on` map toLower . ncString

instance Ord NoCaseString where
  compare = compare `on` map toLower . ncString

instance Show NoCaseString where
  show = show . ncString
