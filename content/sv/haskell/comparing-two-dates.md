---
title:                "Jämför två datum"
html_title:           "Arduino: Jämför två datum"
simple_title:         "Jämför två datum"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/haskell/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att jämföra två datum handlar om att bestämma vilket datum som kommer före eller efter det andra. Programmerare gör det för att delvis hantera händelser och tidslinjer i applikationer.

## Hur man gör:
Här är ett enkelt exempel på hur man jämför två datum i Haskell:

```Haskell
import Data.Time

dateCompare :: IO ()
dateCompare = do
  let date1 = fromGregorian 2020 5 17 
  let date2 = fromGregorian 2021 5 17
  print (date1 < date2)
```
När du kör denna kod skulle utskriften bli `True` eftersom `date1` (17 maj 2020) är tidigare än `date2` (17 maj 2021).

## Fördjupning
Jämförelse av datum är en fundamental operation och har funnits lika länge som programmeringsspråk. Det finns också flera alternativ för jämförelse av datum, speciellt med bibliotek som `time` eller `datetime`. Haskell använder `Data.Time` modulen för att hantera datum och tid.

Haskell jämför två datum med hjälp av standardsoperatörerna (>, <, ==, etc). Innan jämförelsen omvandlar Haskell datumen till den interna representationen av dagsantalet sedan början av den gregorianska kalendern. Detta gör jämförelsen både snabb och exakt.

## Se även
För mer information om datumhantering i Haskell, besök följande länkar:

- Haskell's [`Data.Time` module documentation](https://hackage.haskell.org/package/time-1.6.0.1/docs/Data-Time.html)
- [A deep dive into Haskell's dates and times on StackOverflow](https://stackoverflow.com/questions/37089029/how-do-i-parse-and-format-dates-in-haskell)