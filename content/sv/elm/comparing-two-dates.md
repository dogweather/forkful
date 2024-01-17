---
title:                "Jämföring av två datum"
html_title:           "Elm: Jämföring av två datum"
simple_title:         "Jämföring av två datum"
programming_language: "Elm"
category:             "Elm"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/elm/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Vad & Varför?

Att jämföra två datum är en vanlig uppgift för programmerare. Det handlar helt enkelt om att bestämma vilket av två datum som ligger före eller efter det andra. Detta kan vara användbart för att sortera data eller för att kontrollera om ett datum ligger inom en viss tidsram.

## Hur man gör:

För att jämföra två datum i Elm, använder man `Date` modulen. Första steget är att konvertera de två datum till `Date` objekt med hjälp av funktionen `Date.fromString`. Sedan kan man använda `Date.compare` för att jämföra de två objekten. Funktionen returnerar en `Order` typ som antingen är `LT` (mindre än), `EQ` (lika med) eller `GT` (större än).

```Elm
import Date exposing (..)

date1 = Date.fromString "2021-01-01"
date2 = Date.fromString "2021-02-01"

result = Date.compare date1 date2 -- LT
```

## Djupdykning:

Att jämföra datum är ett vanligt problem inom programmering och det finns många sätt att lösa det på. En alternativ metod är att jämföra datum som `Int` värden, där varje dag räknas som ett numeriskt värde. Men detta kan bli komplicerat med tanke på skottår och månadslängd.

Elm's `Date` modul använder sig av den gregorianska kalendern och stöder datum upp till år 10 000. Implementeringen är skriven i ren Elm-kod och är därför beroende av andra Elm-moduler som `String` och `List`.

## Se även:

- [Official Elm Date documentation](https://package.elm-lang.org/packages/elm/core/latest/Date)
- [Wikipedia article on Gregorian calendar](https://en.wikipedia.org/wiki/Gregorian_calendar)
- [Elm's source code for Date module](https://github.com/elm/core/blob/master/src/Date.elm)