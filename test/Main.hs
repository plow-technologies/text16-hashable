{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeApplications #-}

module Main (main) where

import Control.Monad
import Data.Hashable (hash)
import qualified Data.Text as T
import Data.Text16 (fromText2)
import Data.Text16.Hashable ()
import Data.Aeson
import System.Exit
import System.IO.Error
import Control.Exception
import Test.QuickCheck
import GHC.Generics



data Sample = Sample
  { value :: T.Text
  , expectedHash :: Int
  } deriving (Eq, Show, Generic, ToJSON, FromJSON)

main :: IO ()
main = do
  let fname = "hashable-golden.json"
  emGolden <- tryJust (guard . isDoesNotExistError) (decodeFileStrict @[Sample] fname)
  case emGolden of
    Right (Just samples) -> do
      putStrLn $ "Checking samples in " <> fname
      allGood <- foldM
        (\allGood (Sample v h) ->
          if hash (fromText2 v) /= h
            then putStrLn ("MISMATCH on " <> show v) >> pure False
            else pure allGood 
        )
        True
        samples
      if allGood then pure () else die "unstable hash"
    Right Nothing -> die "Unparsable golden file"
    Left _ -> do
      putStrLn $ "Saving samples to " <> fname
      encodeFile @[Sample] fname =<< generate (vector 1000)
      
  
instance Arbitrary Sample where
  arbitrary = do
    v <- T.pack <$> (flip vectorOf arbitraryUnicodeChar . getNonNegative =<< arbitrary)
    pure (Sample v (hash (fromText2 v)))
