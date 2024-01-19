---
title:                "Beräkna ett datum i framtiden eller förflutna"
html_title:           "Haskell: Beräkna ett datum i framtiden eller förflutna"
simple_title:         "Beräkna ett datum i framtiden eller förflutna"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/haskell/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Vad & Varför?

Att räkna ut ett datum i framtiden eller förflutet betyder att man beräknar exakt vilket datum det blir om x antal dagar från det angivna datumet. Detta är praktiskt för programmerare när de behöver hålla koll på tidspunkter i applikationer, som t.ex. utgångsdatum för lösenord eller påminnelser för evenemang.

## Så här gör du:

I Haskell kan vi enkelt beräkna ett datum i framtiden eller förflutet genom att använda `addDays` funktionen i `Data.Time` biblioteket:

```Haskell
import Data.Time

main = do
    let datum = fromGregorian 2021 01 01
    print (addDays 5 datum) -- 2021-01-06
    print (addDays (-5) datum)-- 2020-12-27
```

## Djupdykning:

Historiskt sett har kalkylering av datum varit en komplex uppgift på grund av skottår, olika månadslängder och tidsskillnader. Haskell använder dock Gregorian-kalendern för att förenkla beräkningar och hålla reda på tidsramar.

Ett alternativ till Haskell's `addDays` funktionen är att direkt ändra fälten `year`, `month` och `day` i `fromGregorian` funktionen. Men detta kräver extra omsorg för att hantera överflöd och kan bli komplicerat mycket snabbt.

När det gäller genomförande, sköter Haskell det mesta av det tunga lyftandet bakom kulisserna. Den `addDays` funktionen hanterar alla detaljer som skottår och månadslängder, vilket gör det mycket enkelt att använda.

## Se också:

- [Haskell Time Library](http://hackage.haskell.org/package/time-1.11.1.1)
- [Gregoriansk kalender](https://sv.wikipedia.org/wiki/Gregoriansk_kalender)