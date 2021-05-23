{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE Safe #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

-- |
-- Module:      Data.Time.Format.Typed
-- Copyright:   (c) 20021 Gautier DI FOLCO
-- License:     ISC
-- Maintainer:  Gautier DI FOLCO <gautier.difolco@gmail.com>
-- Stability:   experimental
-- Portability: GHC
--
-- Simple type-safe wrapper for <https://hackage.haskell.org/package/time time>'s
-- <https://hackage.haskell.org/package/time-1.11.1.2/docs/Data-Time-Format.html Data.Time.Format>.
--
-- You are are to represent a sound formatting at type-level:
--
--
-- @
-- myInput :: 'FormattedTime' 'RFC822'
-- @
module Data.Time.Format.Typed
  ( -- | Formatting types
    NoPadding,
    SpacesPadding,
    ZerosPadding,
    ToUpperCase,
    ToLowerCase,
    Width,
    Alternate,
    Specifier (..),
    (:<>),
    -- | Format holders
    FormattedTime (..),
    -- | Formatting functions
    formatTime',
    parseTimeM',
    -- | Typeclasses
    Formatter (..),
    Printable,
    Parsable,
    SupportedFormatting,
  )
where

import Data.Kind
import Data.Proxy
import Data.Time.Calendar
import Data.Time.Calendar.Month
import Data.Time.Calendar.WeekDate
import Data.Time.Clock
import Data.Time.Format
import Data.Time.LocalTime
import GHC.TypeLits

-- | Formatted time 'String'
newtype FormattedTime a = FormattedTime {weakFormattedTime :: String}
  deriving (Eq, Show)

-- | Type version of <https://hackage.haskell.org/package/time-1.11.1.2/docs/Data-Time-Format.html#v:formatTime formatTime>
formatTime' ::
  forall f t.
  ( FormatTime t,
    Formatter f,
    Printable f,
    SupportedFormatting t f
  ) =>
  TimeLocale ->
  t ->
  FormattedTime f
formatTime' locale time = FormattedTime $ formatTime locale (getFormat $ Proxy @f) time

-- | Type version of <https://hackage.haskell.org/package/time-1.11.1.2/docs/Data-Time-Format.html#v:parseTimeM parseTimeM>
parseTimeM' ::
  forall m f t.
  ( MonadFail m,
    ParseTime t,
    Formatter f,
    Parsable f,
    SupportedFormatting t f
  ) =>
  Bool ->
  TimeLocale ->
  FormattedTime f ->
  m t
parseTimeM' whitespace locale time = parseTimeM whitespace locale (getFormat $ Proxy @f) (weakFormattedTime time)

-- | == UNIX format representation
--
-- See
-- <https://hackage.haskell.org/package/time-1.11.1.2/docs/Data-Time-Format.html#v:formatTime>

-- | %-z
data NoPadding (a :: k)

-- | %_z
data SpacesPadding (a :: k)

-- | %0z
data ZerosPadding (a :: k)

-- | %^z
data ToUpperCase (a :: k)

-- | %#z
data ToLowerCase (a :: k)

-- | %nz
data Width (w :: Nat) (a :: k)

-- | %Ez
data Alternate (a :: Specifier)

data Specifier
  = -- | %%
    Percent
  | -- | %t
    Tab
  | -- | %n
    Newline
  | -- === ZonedTime & UTCTime

    -- | %z
    TimeZoneOffset
  | -- | %Z
    TimeZoneName
  | -- === LocalTime & ZonedTime & UTCTime & UniversalTime

    -- | %s
    EpochSeconds
  | -- === TimeOfDay & LocalTime & ZonedTime & UTCTime & UniversalTime

    -- | %c
    LocalDateTile
  | -- | %R
    HoursMinutes
  | -- | %T
    HoursMinutesSeconds
  | -- | %X
    LocaleHoursMinutesSeconds
  | -- | %r
    LocaleTwelveHoursMinutesSecondsPicos
  | -- | %p
    LocaleLowercaseDayHalf
  | -- | %P
    LocaleUppercaseDayHalf
  | -- | %H
    PaddedTwentyFourHours
  | -- | %k
    NoPaddedTwentyFourHours
  | -- | %I
    PaddedTwelveHours
  | -- | %l
    NoPaddedTwelveHours
  | -- | %M
    PaddedMinutes
  | -- | %S
    PaddedSeconds
  | -- | %q
    PaddedPicoseconds
  | -- === DayOfWeek & Day & LocalTime & ZonedTime & UTCTime & UniversalTime

    -- | %Q
    SecondsFraction
  | -- | %u
    WeekDayNumberFormat
  | -- | %w
    WeekDayNumber
  | -- | %a
    WeekDayShort
  | -- === Month & Day & LocalTime & ZonedTime & UTCTime & UniversalTime

    -- | %A
    WeekDayLong
  | -- | %Y
    Year
  | -- | %y
    YearCentury
  | -- | %C
    Century
  | -- | %B
    MonthLong
  | -- | %b, %h
    MonthShort
  | -- === Day & LocalTime & ZonedTime & UTCTime & UniversalTime

    -- | %m
    MonthNumber
  | -- | %D
    DateUs
  | -- | %F
    DateIso
  | -- | %x
    DateLocale
  | -- | %d
    DayZeroPadded
  | -- | %e
    DaySpacePadded
  | -- | %j
    DayOfYear
  | -- | %f
    CenturyFormat
  | -- | %V
    WeekOfYearFormat
  | -- | %U
    WeekOfYearNumberSunday
  | -- === NominalDiffTime & DiffTime

    -- | %W
    WeekOfYearNumberMonday
  | -- | %w
    WholeWeeks
  | -- | %d
    WholeDays
  | -- | %D
    WholeDaysOfWeek
  | -- | %h
    WholeHours
  | -- | %H
    WholeHoursOfDay
  | -- | %m
    WholeMinutes
  | -- | %M
    WholeMinutesOfDay
  | -- | %s
    WholeSeconds
  | -- === CalendarDiffDays & CalendarDiffTime

    -- | %S
    WholeSecondsOfDay
  | -- | %y
    DiffYears
  | -- | %b
    DiffMonths
  | -- | %B
    DiffMonthsOfYear
  | -- | %w
    DiffWeeksWithoutMonths
  | -- | %d
    DiffDaysWithoutMonths
  | -- === CalendarDiffTime

    -- | %D
    DiffDaysOfWeek
  | -- | %h
    CalendarDiffHoursWithoutMonths
  | -- | %H
    CalendarDiffHours
  | -- | %m
    CalendarDiffMinutes
  | -- | %M
    CalendarDiffMinutesWithoutMonths
  | -- | %s
    CalendarDiffSecondsWithoutMonths
  | -- === Parsing

    -- | %S
    CalendarDiffSeconds
  | -- | %0Y %0G
    TwoDigits
  | -- | %0C %0f
    FourDigits

-- | Concatenation
data (left :: k0) :<> (right :: k1)

infixr 4 :<>

(<.>) :: String -> String -> String
l <.> r = l <> tail r

class Formatter a where
  getFormat :: Proxy a -> String

instance KnownSymbol a => Formatter a where getFormat _ = symbolVal $ Proxy @a

instance (Formatter a, Formatter b) => Formatter (a :<> b) where getFormat _ = getFormat (Proxy @a) <> getFormat (Proxy @b)

instance Formatter a => Formatter (NoPadding a) where getFormat _ = "%-" <.> getFormat (Proxy @a)

instance Formatter a => Formatter (SpacesPadding a) where getFormat _ = "%_" <.> getFormat (Proxy @a)

instance Formatter a => Formatter (ZerosPadding a) where getFormat _ = "%0" <.> getFormat (Proxy @a)

instance Formatter a => Formatter (ToUpperCase a) where getFormat _ = "%^" <.> getFormat (Proxy @a)

instance Formatter a => Formatter (ToLowerCase a) where getFormat _ = "%#" <.> getFormat (Proxy @a)

instance (KnownNat n, Formatter a) => Formatter (Width n a) where getFormat _ = show (natVal $ Proxy @n) <.> getFormat (Proxy @a)

instance Formatter a => Formatter (Alternate a) where getFormat _ = "%E" <.> getFormat (Proxy @a)

instance Formatter 'Percent where getFormat _ = "%%"

instance Formatter 'Tab where getFormat _ = "%t"

instance Formatter 'Newline where getFormat _ = "%n"

instance Formatter 'TimeZoneOffset where getFormat _ = "%z"

instance Formatter 'TimeZoneName where getFormat _ = "%Z"

instance Formatter 'EpochSeconds where getFormat _ = "%s"

instance Formatter 'LocalDateTile where getFormat _ = "%c"

instance Formatter 'HoursMinutes where getFormat _ = "%R"

instance Formatter 'HoursMinutesSeconds where getFormat _ = "%T"

instance Formatter 'LocaleHoursMinutesSeconds where getFormat _ = "%X"

instance Formatter 'LocaleTwelveHoursMinutesSecondsPicos where getFormat _ = "%r"

instance Formatter 'LocaleLowercaseDayHalf where getFormat _ = "%p"

instance Formatter 'LocaleUppercaseDayHalf where getFormat _ = "%P"

instance Formatter 'PaddedTwentyFourHours where getFormat _ = "%H"

instance Formatter 'NoPaddedTwentyFourHours where getFormat _ = "%k"

instance Formatter 'PaddedTwelveHours where getFormat _ = "%I"

instance Formatter 'NoPaddedTwelveHours where getFormat _ = "%l"

instance Formatter 'PaddedMinutes where getFormat _ = "%M"

instance Formatter 'PaddedSeconds where getFormat _ = "%S"

instance Formatter 'PaddedPicoseconds where getFormat _ = "%q"

instance Formatter 'SecondsFraction where getFormat _ = "%Q"

instance Formatter 'WeekDayNumberFormat where getFormat _ = "%u"

instance Formatter 'WeekDayNumber where getFormat _ = "%w"

instance Formatter 'WeekDayShort where getFormat _ = "%a"

instance Formatter 'WeekDayLong where getFormat _ = "%A"

instance Formatter 'Year where getFormat _ = "%Y"

instance Formatter 'YearCentury where getFormat _ = "%y"

instance Formatter 'Century where getFormat _ = "%C"

instance Formatter 'MonthLong where getFormat _ = "%B"

instance Formatter 'MonthShort where getFormat _ = "%b"

instance Formatter 'MonthNumber where getFormat _ = "%m"

instance Formatter 'DateUs where getFormat _ = "%D"

instance Formatter 'DateIso where getFormat _ = "%F"

instance Formatter 'DateLocale where getFormat _ = "%x"

instance Formatter 'DayZeroPadded where getFormat _ = "%d"

instance Formatter 'DaySpacePadded where getFormat _ = "%e"

instance Formatter 'DayOfYear where getFormat _ = "%j"

instance Formatter 'CenturyFormat where getFormat _ = "%f"

instance Formatter 'WeekOfYearFormat where getFormat _ = "%V"

instance Formatter 'WeekOfYearNumberSunday where getFormat _ = "%U"

instance Formatter 'WeekOfYearNumberMonday where getFormat _ = "%W"

instance Formatter 'WholeWeeks where getFormat _ = "%w"

instance Formatter 'WholeDays where getFormat _ = "%d"

instance Formatter 'WholeDaysOfWeek where getFormat _ = "%D"

instance Formatter 'WholeHours where getFormat _ = "%h"

instance Formatter 'WholeHoursOfDay where getFormat _ = "%H"

instance Formatter 'WholeMinutes where getFormat _ = "%m"

instance Formatter 'WholeMinutesOfDay where getFormat _ = "%M"

instance Formatter 'WholeSeconds where getFormat _ = "%s"

instance Formatter 'WholeSecondsOfDay where getFormat _ = "%S"

instance Formatter 'DiffYears where getFormat _ = "%y"

instance Formatter 'DiffMonths where getFormat _ = "%b"

instance Formatter 'DiffMonthsOfYear where getFormat _ = "%B"

instance Formatter 'DiffWeeksWithoutMonths where getFormat _ = "%w"

instance Formatter 'DiffDaysWithoutMonths where getFormat _ = "%d"

instance Formatter 'DiffDaysOfWeek where getFormat _ = "%D"

instance Formatter 'CalendarDiffHoursWithoutMonths where getFormat _ = "%h"

instance Formatter 'CalendarDiffHours where getFormat _ = "%H"

instance Formatter 'CalendarDiffMinutes where getFormat _ = "%m"

instance Formatter 'CalendarDiffMinutesWithoutMonths where getFormat _ = "%M"

instance Formatter 'CalendarDiffSecondsWithoutMonths where getFormat _ = "%s"

instance Formatter 'CalendarDiffSeconds where getFormat _ = "%S"

instance Formatter 'TwoDigits where getFormat _ = "%Y"

instance Formatter 'FourDigits where getFormat _ = "%C"

class Printable a

instance KnownSymbol n => Printable n

instance (Printable a, Printable b) => Printable (a :<> b)

instance Printable a => Printable (NoPadding a)

instance Printable a => Printable (SpacesPadding a)

instance Printable a => Printable (ZerosPadding a)

instance Printable a => Printable (ToUpperCase a)

instance Printable a => Printable (ToLowerCase a)

instance Printable a => Printable (Width n a)

instance Printable a => Printable (Alternate a)

instance Printable 'Percent

instance Printable 'Tab

instance Printable 'Newline

instance Printable 'TimeZoneOffset

instance Printable 'TimeZoneName

instance Printable 'EpochSeconds

instance Printable 'LocalDateTile

instance Printable 'HoursMinutes

instance Printable 'HoursMinutesSeconds

instance Printable 'LocaleHoursMinutesSeconds

instance Printable 'LocaleTwelveHoursMinutesSecondsPicos

instance Printable 'LocaleLowercaseDayHalf

instance Printable 'LocaleUppercaseDayHalf

instance Printable 'PaddedTwentyFourHours

instance Printable 'NoPaddedTwentyFourHours

instance Printable 'PaddedTwelveHours

instance Printable 'NoPaddedTwelveHours

instance Printable 'PaddedMinutes

instance Printable 'PaddedSeconds

instance Printable 'PaddedPicoseconds

instance Printable 'SecondsFraction

instance Printable 'WeekDayNumberFormat

instance Printable 'WeekDayNumber

instance Printable 'WeekDayShort

instance Printable 'WeekDayLong

instance Printable 'Year

instance Printable 'YearCentury

instance Printable 'Century

instance Printable 'MonthLong

instance Printable 'MonthShort

instance Printable 'MonthNumber

instance Printable 'DateUs

instance Printable 'DateIso

instance Printable 'DateLocale

instance Printable 'DayZeroPadded

instance Printable 'DaySpacePadded

instance Printable 'DayOfYear

instance Printable 'CenturyFormat

instance Printable 'WeekOfYearFormat

instance Printable 'WeekOfYearNumberSunday

instance Printable 'WeekOfYearNumberMonday

instance Printable 'WholeWeeks

instance Printable 'WholeDays

instance Printable 'WholeDaysOfWeek

instance Printable 'WholeHours

instance Printable 'WholeHoursOfDay

instance Printable 'WholeMinutes

instance Printable 'WholeMinutesOfDay

instance Printable 'WholeSeconds

instance Printable 'WholeSecondsOfDay

instance Printable 'DiffYears

instance Printable 'DiffMonths

instance Printable 'DiffMonthsOfYear

instance Printable 'DiffWeeksWithoutMonths

instance Printable 'DiffDaysWithoutMonths

instance Printable 'DiffDaysOfWeek

instance Printable 'CalendarDiffHoursWithoutMonths

instance Printable 'CalendarDiffHours

instance Printable 'CalendarDiffMinutes

instance Printable 'CalendarDiffMinutesWithoutMonths

instance Printable 'CalendarDiffSecondsWithoutMonths

instance Printable 'CalendarDiffSeconds

class Parsable a

instance KnownSymbol n => Parsable n

instance (Parsable a, Parsable b) => Parsable (a :<> b)

instance Parsable a => Parsable (NoPadding a)

instance Parsable a => Parsable (SpacesPadding a)

instance Parsable a => Parsable (ZerosPadding a)

instance Parsable a => Parsable (ToUpperCase a)

instance Parsable a => Parsable (ToLowerCase a)

instance Parsable a => Parsable (Width n a)

instance Parsable a => Parsable (Alternate a)

instance Parsable 'Percent

instance Parsable 'Tab

instance Parsable 'Newline

instance Parsable 'TimeZoneOffset

instance Parsable 'TimeZoneName

instance Parsable 'EpochSeconds

instance Parsable 'LocalDateTile

instance Parsable 'HoursMinutes

instance Parsable 'HoursMinutesSeconds

instance Parsable 'LocaleHoursMinutesSeconds

instance Parsable 'LocaleTwelveHoursMinutesSecondsPicos

instance Parsable 'LocaleLowercaseDayHalf

instance Parsable 'LocaleUppercaseDayHalf

instance Parsable 'PaddedTwentyFourHours

instance Parsable 'NoPaddedTwentyFourHours

instance Parsable 'PaddedTwelveHours

instance Parsable 'NoPaddedTwelveHours

instance Parsable 'PaddedMinutes

instance Parsable 'PaddedSeconds

instance Parsable 'PaddedPicoseconds

instance Parsable 'SecondsFraction

instance Parsable 'WeekDayNumberFormat

instance Parsable 'WeekDayNumber

instance Parsable 'WeekDayShort

instance Parsable 'WeekDayLong

instance Parsable 'Year

instance Parsable 'YearCentury

instance Parsable 'Century

instance Parsable 'MonthLong

instance Parsable 'MonthShort

instance Parsable 'MonthNumber

instance Parsable 'DateUs

instance Parsable 'DateIso

instance Parsable 'DateLocale

instance Parsable 'DayZeroPadded

instance Parsable 'DaySpacePadded

instance Parsable 'DayOfYear

instance Parsable 'CenturyFormat

instance Parsable 'WeekOfYearFormat

instance Parsable 'WeekOfYearNumberSunday

instance Parsable 'WeekOfYearNumberMonday

instance Parsable 'WholeWeeks

instance Parsable 'WholeDays

instance Parsable 'WholeDaysOfWeek

instance Parsable 'WholeHours

instance Parsable 'WholeHoursOfDay

instance Parsable 'WholeMinutes

instance Parsable 'WholeMinutesOfDay

instance Parsable 'WholeSeconds

instance Parsable 'WholeSecondsOfDay

instance Parsable 'DiffYears

instance Parsable 'DiffMonths

instance Parsable 'DiffMonthsOfYear

instance Parsable 'DiffWeeksWithoutMonths

instance Parsable 'DiffDaysWithoutMonths

instance Parsable 'DiffDaysOfWeek

instance Parsable 'CalendarDiffHoursWithoutMonths

instance Parsable 'CalendarDiffHours

instance Parsable 'CalendarDiffMinutes

instance Parsable 'CalendarDiffMinutesWithoutMonths

instance Parsable 'CalendarDiffSecondsWithoutMonths

instance Parsable 'CalendarDiffSeconds

instance Parsable 'TwoDigits

instance Parsable 'FourDigits

class SupportedFormatting t f

instance KnownSymbol n => SupportedFormatting t n

instance (SupportedFormatting t a, SupportedFormatting t b) => SupportedFormatting t (a :<> b)

instance SupportedFormatting t a => SupportedFormatting t (NoPadding a)

instance SupportedFormatting t a => SupportedFormatting t (SpacesPadding a)

instance SupportedFormatting t a => SupportedFormatting t (ZerosPadding a)

instance SupportedFormatting t a => SupportedFormatting t (ToUpperCase a)

instance SupportedFormatting t a => SupportedFormatting t (ToLowerCase a)

instance SupportedFormatting t a => SupportedFormatting t (Width n a)

instance SupportedFormatting t a => SupportedFormatting t (Alternate a)

instance SupportedFormatting t 'Percent

instance SupportedFormatting t 'Tab

instance SupportedFormatting t 'Newline

instance SupportedFormatting ZonedTime 'TimeZoneOffset

instance SupportedFormatting ZonedTime 'TimeZoneName

instance SupportedFormatting ZonedTime 'EpochSeconds

instance SupportedFormatting UTCTime 'TimeZoneOffset

instance SupportedFormatting UTCTime 'TimeZoneName

instance SupportedFormatting UTCTime 'EpochSeconds

instance SupportedFormatting LocalTime 'LocalDateTile

instance SupportedFormatting ZonedTime 'LocalDateTile

instance SupportedFormatting UTCTime 'LocalDateTile

instance SupportedFormatting UniversalTime 'LocalDateTile

instance SupportedFormatting TimeOfDay 'HoursMinutes

instance SupportedFormatting TimeOfDay 'HoursMinutesSeconds

instance SupportedFormatting TimeOfDay 'LocaleHoursMinutesSeconds

instance SupportedFormatting TimeOfDay 'LocaleTwelveHoursMinutesSecondsPicos

instance SupportedFormatting TimeOfDay 'LocaleLowercaseDayHalf

instance SupportedFormatting TimeOfDay 'LocaleUppercaseDayHalf

instance SupportedFormatting TimeOfDay 'PaddedTwentyFourHours

instance SupportedFormatting TimeOfDay 'NoPaddedTwentyFourHours

instance SupportedFormatting TimeOfDay 'PaddedTwelveHours

instance SupportedFormatting TimeOfDay 'NoPaddedTwelveHours

instance SupportedFormatting TimeOfDay 'PaddedMinutes

instance SupportedFormatting TimeOfDay 'PaddedSeconds

instance SupportedFormatting TimeOfDay 'PaddedPicoseconds

instance SupportedFormatting TimeOfDay 'SecondsFraction

instance SupportedFormatting LocalTime 'HoursMinutes

instance SupportedFormatting LocalTime 'HoursMinutesSeconds

instance SupportedFormatting LocalTime 'LocaleHoursMinutesSeconds

instance SupportedFormatting LocalTime 'LocaleTwelveHoursMinutesSecondsPicos

instance SupportedFormatting LocalTime 'LocaleLowercaseDayHalf

instance SupportedFormatting LocalTime 'LocaleUppercaseDayHalf

instance SupportedFormatting LocalTime 'PaddedTwentyFourHours

instance SupportedFormatting LocalTime 'NoPaddedTwentyFourHours

instance SupportedFormatting LocalTime 'PaddedTwelveHours

instance SupportedFormatting LocalTime 'NoPaddedTwelveHours

instance SupportedFormatting LocalTime 'PaddedMinutes

instance SupportedFormatting LocalTime 'PaddedSeconds

instance SupportedFormatting LocalTime 'PaddedPicoseconds

instance SupportedFormatting LocalTime 'SecondsFraction

instance SupportedFormatting ZonedTime 'HoursMinutes

instance SupportedFormatting ZonedTime 'HoursMinutesSeconds

instance SupportedFormatting ZonedTime 'LocaleHoursMinutesSeconds

instance SupportedFormatting ZonedTime 'LocaleTwelveHoursMinutesSecondsPicos

instance SupportedFormatting ZonedTime 'LocaleLowercaseDayHalf

instance SupportedFormatting ZonedTime 'LocaleUppercaseDayHalf

instance SupportedFormatting ZonedTime 'PaddedTwentyFourHours

instance SupportedFormatting ZonedTime 'NoPaddedTwentyFourHours

instance SupportedFormatting ZonedTime 'PaddedTwelveHours

instance SupportedFormatting ZonedTime 'NoPaddedTwelveHours

instance SupportedFormatting ZonedTime 'PaddedMinutes

instance SupportedFormatting ZonedTime 'PaddedSeconds

instance SupportedFormatting ZonedTime 'PaddedPicoseconds

instance SupportedFormatting ZonedTime 'SecondsFraction

instance SupportedFormatting UTCTime 'HoursMinutes

instance SupportedFormatting UTCTime 'HoursMinutesSeconds

instance SupportedFormatting UTCTime 'LocaleHoursMinutesSeconds

instance SupportedFormatting UTCTime 'LocaleTwelveHoursMinutesSecondsPicos

instance SupportedFormatting UTCTime 'LocaleLowercaseDayHalf

instance SupportedFormatting UTCTime 'LocaleUppercaseDayHalf

instance SupportedFormatting UTCTime 'PaddedTwentyFourHours

instance SupportedFormatting UTCTime 'NoPaddedTwentyFourHours

instance SupportedFormatting UTCTime 'PaddedTwelveHours

instance SupportedFormatting UTCTime 'NoPaddedTwelveHours

instance SupportedFormatting UTCTime 'PaddedMinutes

instance SupportedFormatting UTCTime 'PaddedSeconds

instance SupportedFormatting UTCTime 'PaddedPicoseconds

instance SupportedFormatting UTCTime 'SecondsFraction

instance SupportedFormatting UniversalTime 'HoursMinutes

instance SupportedFormatting UniversalTime 'HoursMinutesSeconds

instance SupportedFormatting UniversalTime 'LocaleHoursMinutesSeconds

instance SupportedFormatting UniversalTime 'LocaleTwelveHoursMinutesSecondsPicos

instance SupportedFormatting UniversalTime 'LocaleLowercaseDayHalf

instance SupportedFormatting UniversalTime 'LocaleUppercaseDayHalf

instance SupportedFormatting UniversalTime 'PaddedTwentyFourHours

instance SupportedFormatting UniversalTime 'NoPaddedTwentyFourHours

instance SupportedFormatting UniversalTime 'PaddedTwelveHours

instance SupportedFormatting UniversalTime 'NoPaddedTwelveHours

instance SupportedFormatting UniversalTime 'PaddedMinutes

instance SupportedFormatting UniversalTime 'PaddedSeconds

instance SupportedFormatting UniversalTime 'PaddedPicoseconds

instance SupportedFormatting UniversalTime 'SecondsFraction

instance SupportedFormatting DayOfWeek 'WeekDayNumberFormat

instance SupportedFormatting DayOfWeek 'WeekDayNumber

instance SupportedFormatting DayOfWeek 'WeekDayShort

instance SupportedFormatting DayOfWeek 'WeekDayLong

instance SupportedFormatting Day 'WeekDayNumberFormat

instance SupportedFormatting Day 'WeekDayNumber

instance SupportedFormatting Day 'WeekDayShort

instance SupportedFormatting Day 'WeekDayLong

instance SupportedFormatting LocalTime 'WeekDayNumberFormat

instance SupportedFormatting LocalTime 'WeekDayNumber

instance SupportedFormatting LocalTime 'WeekDayShort

instance SupportedFormatting LocalTime 'WeekDayLong

instance SupportedFormatting ZonedTime 'WeekDayNumberFormat

instance SupportedFormatting ZonedTime 'WeekDayNumber

instance SupportedFormatting ZonedTime 'WeekDayShort

instance SupportedFormatting ZonedTime 'WeekDayLong

instance SupportedFormatting UTCTime 'WeekDayNumberFormat

instance SupportedFormatting UTCTime 'WeekDayNumber

instance SupportedFormatting UTCTime 'WeekDayShort

instance SupportedFormatting UTCTime 'WeekDayLong

instance SupportedFormatting UniversalTime 'WeekDayNumberFormat

instance SupportedFormatting UniversalTime 'WeekDayNumber

instance SupportedFormatting UniversalTime 'WeekDayShort

instance SupportedFormatting UniversalTime 'WeekDayLong

instance SupportedFormatting Month 'Year

instance SupportedFormatting Month 'YearCentury

instance SupportedFormatting Month 'Century

instance SupportedFormatting Month 'MonthLong

instance SupportedFormatting Month 'MonthShort

instance SupportedFormatting Month 'MonthNumber

instance SupportedFormatting Day 'Year

instance SupportedFormatting Day 'YearCentury

instance SupportedFormatting Day 'Century

instance SupportedFormatting Day 'MonthLong

instance SupportedFormatting Day 'MonthShort

instance SupportedFormatting Day 'MonthNumber

instance SupportedFormatting LocalTime 'Year

instance SupportedFormatting LocalTime 'YearCentury

instance SupportedFormatting LocalTime 'Century

instance SupportedFormatting LocalTime 'MonthLong

instance SupportedFormatting LocalTime 'MonthShort

instance SupportedFormatting LocalTime 'MonthNumber

instance SupportedFormatting ZonedTime 'Year

instance SupportedFormatting ZonedTime 'YearCentury

instance SupportedFormatting ZonedTime 'Century

instance SupportedFormatting ZonedTime 'MonthLong

instance SupportedFormatting ZonedTime 'MonthShort

instance SupportedFormatting ZonedTime 'MonthNumber

instance SupportedFormatting UTCTime 'Year

instance SupportedFormatting UTCTime 'YearCentury

instance SupportedFormatting UTCTime 'Century

instance SupportedFormatting UTCTime 'MonthLong

instance SupportedFormatting UTCTime 'MonthShort

instance SupportedFormatting UTCTime 'MonthNumber

instance SupportedFormatting UniversalTime 'Year

instance SupportedFormatting UniversalTime 'YearCentury

instance SupportedFormatting UniversalTime 'Century

instance SupportedFormatting UniversalTime 'MonthLong

instance SupportedFormatting UniversalTime 'MonthShort

instance SupportedFormatting UniversalTime 'MonthNumber

instance SupportedFormatting Day 'DateUs

instance SupportedFormatting Day 'DateIso

instance SupportedFormatting Day 'DateLocale

instance SupportedFormatting Day 'DayZeroPadded

instance SupportedFormatting Day 'DaySpacePadded

instance SupportedFormatting Day 'DayOfYear

instance SupportedFormatting Day 'CenturyFormat

instance SupportedFormatting Day 'WeekOfYearFormat

instance SupportedFormatting Day 'WeekOfYearNumberSunday

instance SupportedFormatting Day 'WeekOfYearNumberMonday

instance SupportedFormatting LocalTime 'DateUs

instance SupportedFormatting LocalTime 'DateIso

instance SupportedFormatting LocalTime 'DateLocale

instance SupportedFormatting LocalTime 'DayZeroPadded

instance SupportedFormatting LocalTime 'DaySpacePadded

instance SupportedFormatting LocalTime 'DayOfYear

instance SupportedFormatting LocalTime 'CenturyFormat

instance SupportedFormatting LocalTime 'WeekOfYearFormat

instance SupportedFormatting LocalTime 'WeekOfYearNumberSunday

instance SupportedFormatting LocalTime 'WeekOfYearNumberMonday

instance SupportedFormatting ZonedTime 'DateUs

instance SupportedFormatting ZonedTime 'DateIso

instance SupportedFormatting ZonedTime 'DateLocale

instance SupportedFormatting ZonedTime 'DayZeroPadded

instance SupportedFormatting ZonedTime 'DaySpacePadded

instance SupportedFormatting ZonedTime 'DayOfYear

instance SupportedFormatting ZonedTime 'CenturyFormat

instance SupportedFormatting ZonedTime 'WeekOfYearFormat

instance SupportedFormatting ZonedTime 'WeekOfYearNumberSunday

instance SupportedFormatting ZonedTime 'WeekOfYearNumberMonday

instance SupportedFormatting UTCTime 'DateUs

instance SupportedFormatting UTCTime 'DateIso

instance SupportedFormatting UTCTime 'DateLocale

instance SupportedFormatting UTCTime 'DayZeroPadded

instance SupportedFormatting UTCTime 'DaySpacePadded

instance SupportedFormatting UTCTime 'DayOfYear

instance SupportedFormatting UTCTime 'CenturyFormat

instance SupportedFormatting UTCTime 'WeekOfYearFormat

instance SupportedFormatting UTCTime 'WeekOfYearNumberSunday

instance SupportedFormatting UTCTime 'WeekOfYearNumberMonday

instance SupportedFormatting UniversalTime 'DateUs

instance SupportedFormatting UniversalTime 'DateIso

instance SupportedFormatting UniversalTime 'DateLocale

instance SupportedFormatting UniversalTime 'DayZeroPadded

instance SupportedFormatting UniversalTime 'DaySpacePadded

instance SupportedFormatting UniversalTime 'DayOfYear

instance SupportedFormatting UniversalTime 'CenturyFormat

instance SupportedFormatting UniversalTime 'WeekOfYearFormat

instance SupportedFormatting UniversalTime 'WeekOfYearNumberSunday

instance SupportedFormatting UniversalTime 'WeekOfYearNumberMonday

instance SupportedFormatting NominalDiffTime 'WholeWeeks

instance SupportedFormatting NominalDiffTime 'WholeDays

instance SupportedFormatting NominalDiffTime 'WholeDaysOfWeek

instance SupportedFormatting NominalDiffTime 'WholeHours

instance SupportedFormatting NominalDiffTime 'WholeHoursOfDay

instance SupportedFormatting NominalDiffTime 'WholeMinutes

instance SupportedFormatting NominalDiffTime 'WholeMinutesOfDay

instance SupportedFormatting NominalDiffTime 'WholeSeconds

instance SupportedFormatting NominalDiffTime 'WholeSecondsOfDay

instance SupportedFormatting DiffTime 'WholeWeeks

instance SupportedFormatting DiffTime 'WholeDays

instance SupportedFormatting DiffTime 'WholeDaysOfWeek

instance SupportedFormatting DiffTime 'WholeHours

instance SupportedFormatting DiffTime 'WholeHoursOfDay

instance SupportedFormatting DiffTime 'WholeMinutes

instance SupportedFormatting DiffTime 'WholeMinutesOfDay

instance SupportedFormatting DiffTime 'WholeSeconds

instance SupportedFormatting DiffTime 'WholeSecondsOfDay

instance SupportedFormatting CalendarDiffDays 'DiffYears

instance SupportedFormatting CalendarDiffDays 'DiffMonths

instance SupportedFormatting CalendarDiffDays 'DiffMonthsOfYear

instance SupportedFormatting CalendarDiffDays 'DiffWeeksWithoutMonths

instance SupportedFormatting CalendarDiffDays 'DiffDaysWithoutMonths

instance SupportedFormatting CalendarDiffDays 'DiffDaysOfWeek

instance SupportedFormatting CalendarDiffTime 'DiffYears

instance SupportedFormatting CalendarDiffTime 'DiffMonths

instance SupportedFormatting CalendarDiffTime 'DiffMonthsOfYear

instance SupportedFormatting CalendarDiffTime 'DiffWeeksWithoutMonths

instance SupportedFormatting CalendarDiffTime 'DiffDaysWithoutMonths

instance SupportedFormatting CalendarDiffTime 'DiffDaysOfWeek

instance SupportedFormatting CalendarDiffTime 'CalendarDiffHoursWithoutMonths

instance SupportedFormatting CalendarDiffTime 'CalendarDiffHours

instance SupportedFormatting CalendarDiffTime 'CalendarDiffMinutes

instance SupportedFormatting CalendarDiffTime 'CalendarDiffMinutesWithoutMonths

instance SupportedFormatting CalendarDiffTime 'CalendarDiffSecondsWithoutMonths

instance SupportedFormatting CalendarDiffTime 'CalendarDiffSeconds

instance SupportedFormatting a 'TwoDigits

instance SupportedFormatting a 'FourDigits
