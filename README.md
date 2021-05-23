# typed-time
Simple type-safe wrapper for [time](https://hackage.haskell.org/package/time)'s
[Data.Time.Format](https://hackage.haskell.org/package/time-1.11.1.2/docs/Data-Time-Format.html).

You are are to represent a sound formatting at type-level:


```
myInput :: FormattedTime 'RFC822'
```
