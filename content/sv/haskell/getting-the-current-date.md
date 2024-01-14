---
title:                "Haskell: Hämta aktuellt datum"
simple_title:         "Hämta aktuellt datum"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/haskell/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Varför
Att veta vilket datum det är kan vara en viktig del av programmering, speciellt om du behöver hantera tidsbegränsade uppgifter eller skapa tidsstämplar för data. I Haskell finns det en enkel funktion som hämtar det aktuella datumet, vilket gör det enkelt att lägga till denna funktionalitet i dina program.

## Hur man gör
För att få det aktuella datumet i Haskell behöver du importera biblioteket `Data.Time` och använda funktionen `getCurrentTime`. Här är ett exempel på hur du kan göra det:

```Haskell
import Data.Time

main = do
  currentTime <- getCurrentTime
  let currentDate = utctDay currentTime
  putStrLn $ show currentDate
```

Detta kommer att ge dig ett utdata som ser ut så här:

```
2021-09-01
```

Som du kan se, använder vi `utctDay` för att få bara datumet ur `currentTime`, eftersom `getCurrentTime` ger oss en tidsstämpel med både datum och tid.

## Djupdykning
Under huven använder `getCurrentTime` faktiskt `IO` monaden för att hämta det aktuella datumet från systemklockan. Detta innebär att det inte bara är en enkel funktion, utan den hanterar också interaktion med systemet. Men tack vare Haskell's typsystem behöver du inte oroa dig för några oönskade biverkningar när du använder denna funktion.

En annan intressant sak att notera är att funktionen `getCurrentTime` ger oss ett värde av typen `UTCTime` istället för bara ett datum. Detta beror på att Haskell's tidsmodell inkluderar både datum och tid, men om du bara är intresserad av datumet kan du använda `utctDay` för att extrahera den delen.

## Se även
- Haskell dokumentation för `Data.Time` biblioteket: https://hackage.haskell.org/package/time/docs/Data-Time.html
- En bra tutorial om hur man hanterar tider och datum i Haskell: https://mmhaskell.com/blog/2019/11/23/haskell-and-time-series
- Officiell Haskell hemsida för att lära dig mer om språket: https://www.haskell.org/