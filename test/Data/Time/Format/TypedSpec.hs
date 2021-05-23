{-# LANGUAGE TypeApplications #-}

module Data.Time.Format.TypedSpec (spec) where

import Data.Proxy
import Data.Time.Format
import Data.Time.Format.Typed
import Data.Time.Format.Typed.Formats
import Data.Time.LocalTime
import Test.Hspec

spec :: Spec
spec = do
  describe "Formats" $ do
    it "RFC822" $
      getFormat (Proxy @RFC822) `shouldBe` rfc822DateFormat
  describe "formatTime'" $ do
    it "RFC822" $ do
      time <- getZonedTime
      weakFormattedTime (formatTime' @RFC822 defaultTimeLocale time)
        `shouldBe` formatTime defaultTimeLocale rfc822DateFormat time
  describe "parseTimeM'" $ do
    it "RFC822" $ do
      time <- getZonedTime
      let formattedTime = formatTime' @RFC822 defaultTimeLocale time
      fmap show (parseTimeM' @Maybe @RFC822 @ZonedTime True defaultTimeLocale formattedTime)
        `shouldBe` fmap show (parseTimeM @Maybe @ZonedTime True defaultTimeLocale rfc822DateFormat (weakFormattedTime formattedTime))
