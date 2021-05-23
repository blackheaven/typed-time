{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE Safe #-}
{-# LANGUAGE TypeOperators #-}

-- |
-- Module:      Data.Time.Format.Typed.Formats
-- Copyright:   (c) 20021 Gautier DI FOLCO
-- License:     ISC
-- Maintainer:  Gautier DI FOLCO <gautier.difolco@gmail.com>
-- Stability:   experimental
-- Portability: GHC
--
-- Some standard formats
module Data.Time.Format.Typed.Formats
  ( RFC822,
  )
where

import Data.Time.Format.Typed

-- | Format string according to <http://tools.ietf.org/html/rfc822#section-5 RFC822>.
type RFC822 =
  'WeekDayShort
    :<> ", "
    :<> SpacesPadding 'DayZeroPadded
    :<> " "
    :<> 'MonthShort
    :<> " "
    :<> 'Year
    :<> " "
    :<> 'PaddedTwentyFourHours
    :<> ":"
    :<> 'PaddedMinutes
    :<> ":"
    :<> 'PaddedSeconds
    :<> " "
    :<> 'TimeZoneName
