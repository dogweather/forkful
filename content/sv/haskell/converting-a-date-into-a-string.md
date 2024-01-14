---
title:                "Haskell: Omvandla ett datum till en sträng"
simple_title:         "Omvandla ett datum till en sträng"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/haskell/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Varför
Att kunna konvertera ett datum till en sträng kan vara användbart för att visa datum i ett läsbart format, spara datum i en databas eller för att utföra beräkningar baserade på datum.

## Så här gör du
För att konvertera ett datum till en sträng i Haskell behöver du först importera "Data.Time" biblioteket. Sedan kan du använda funktionen `formatTime` för att specificera formatet på den önskade strängen. Här är ett exempel på hur du skulle kunna konvertera dagens datum till en sträng:

```Haskell
import Data.Time

main = do
  today <- getCurrentTime
  let dateString = formatTime defaultTimeLocale "%d/%m/%Y" today
  putStrLn dateString
```

Detta skulle ge följande output:

```
23/05/2021
```

## Djupdykning
När man konverterar ett datum till en sträng är det viktigt att förstå formatet som man vill ha på strängen. I exemplet ovan använde vi `%d` för att få dagens datum och `%m` för att få månadens numeriska värde. Men hur vet man vilka tecken man ska använda för att få det önskade resultatet?

En användbar resurs för att lära sig om olika formateringsalternativ är [Haskell's Data.Time dokumentation](https://hackage.haskell.org/package/time/docs/Data-Time-Format.html). Detta dokument beskriver alla tillgängliga formatteringsalternativ för att konvertera datum till strängar.

Det är också viktigt att förstå att resultatet av `formatTime` funktionen kommer att vara en `String` i Haskell, vilket innebär att du kan använda den på samma sätt som du skulle använda andra strängar i ditt program.

## Se även
* [Data.Time dokumentation](https://hackage.haskell.org/package/time/docs/Data-Time.html)
* [Haskell's standard bibliotek](https://www.haskell.org/onlinereport/standard-prelude.html) (sektion 6.6 för information om `formatTime` funktionen)