---
title:                "Comparing two dates"
date:                  2024-01-20T17:32:58.239587-07:00
model:                 gpt-4-1106-preview
simple_title:         "Comparing two dates"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/haskell/comparing-two-dates.md"
---

{{< edit_this_page >}}

## What & Why?

Comparing two dates means judging which one's earlier, later, or if they're the same moment in time. Programmers do it to sort events, figure out durations, and manage time-dependent logic.

## How to:

Haskell, quietly known for its purity, needs you to talk date-talk with the right libraries. Let's use `Data.Time`.

```haskell
import Data.Time

-- Define two dates
date1 :: UTCTime
date1 = UTCTime (fromGregorian 2023 4 1) (secondsToDiffTime 0)

date2 :: UTCTime
date2 = UTCTime (fromGregorian 2024 4 2) (secondsToDiffTime 3600)

-- Compare the dates
compareDates :: UTCTime -> UTCTime -> Ordering
compareDates = compare

main :: IO ()
main = do
    print $ date1 `compareDates` date2 -- Output will be LT
    print $ date2 `compareDates` date1 -- Output will be GT
    print $ date1 `compareDates` date1 -- Output will be EQ
```

Straightforward, right? `LT` for less than, `GT` for greater than, and `EQ` for equal.

## Deep Dive

Back in the day, Haskell's time handling wasn't as slick. We owe our current comforts to the `Data.Time` library progression over the years. It gives us `UTCTime`, a happily unambiguous point in time.

Alternatives? Sure. You might find `Data.Time.Calendar` and `Data.Time.Clock` useful for specific scenarios. There's also the old `time` library for those feeling nostalgic or stuck with legacy code.

Now, the nitty-gritty: Comparing dates in Haskell hinges on `UTCTime` which pairs a day (`Day`) and a time (`DiffTime` or `NominalDiffTime`). It's the `compare` function doing the heavy lifting, a neat member of the `Ord` class, letting us use `>, <, ==` and more. Just remember Haskell loves its type safety. Ensure you're always comparing apples with apples, or in our case, `UTCTime` with `UTCTime`.

## See Also

Dive deeper or find help with these:
- [`Data.Time` package on Hackage](https://hackage.haskell.org/package/time-1.11/docs/Data-Time.html)
- [Learn You a Haskell for Great Good! – For a gentle introduction](http://learnyouahaskell.com/)
- [Stack Overflow for real-world problem solving](https://stackoverflow.com/questions/tagged/haskell+time)
