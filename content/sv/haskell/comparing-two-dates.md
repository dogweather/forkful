---
title:    "Haskell: Jämföra två datum"
keywords: ["Haskell"]
---

{{< edit_this_page >}}

# Varför

Att jämföra två datum kan vara en viktig del av många Haskell-program. Detta kan göras för att kontrollera giltigheten av data eller för att sortera listor efter datum.

## Hur man gör det

För att börja jämföra två datum i Haskell behöver vi först importera "Data.Time" biblioteket.

```Haskell
import Data.Time
```

Sedan kan vi skapa två "Day" objekt som representerar våra datum som vi vill jämföra.

```Haskell
let datum1 = fromGregorian 2020 10 23
let datum2 = fromGregorian 2020 10 24
```

För att jämföra dessa två datum kan vi använda funktionen "compare", som jämför två objekt av typen "Ord".

```Haskell
compare datum1 datum2
```

Resultatet av denna funktion är antingen "LT" (mindre än), "GT" (större än) eller "EQ" (lika med). I vårt exempel blir resultatet "LT" eftersom "datum1" är mindre än "datum2".

## Deep Dive

I Haskell finns flera olika typer som kan representera datum och tid. Förutom "Day" som används i exemplet ovan, finns det också "TimeOfDay" för specifika tider, "LocalTime" för datum och tid baserat på en viss tidszon, och "UTCTime" för koordinerad universal tid. Det är viktigt att känna till vilken typ av datum du arbetar med när du jämför dem.

När det gäller att jämföra tider finns det även funktionen "diffDays" som kan användas för att få antalet dagar mellan två datum.

## Se även

- https://hackage.haskell.org/package/time-1.9.3/docs/Data-Time.html
- https://wiki.haskell.org/Time_library
- https://dev.to/cgarbin/date-and-time-programming-in-haskell-1608