---
title:                "Haskell: Jämförande av två datum"
programming_language: "Haskell"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/haskell/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Varför

I programutveckling är det ofta nödvändigt att jämföra olika datum för att hantera tidsbaserade funktioner och uppgifter. Genom att lära sig hur man jämför datum i Haskell kan du effektivt hantera dessa typer av uppgifter och undvika problem som felaktiga beräkningar och felaktiga data.

## Så här gör du

För att jämföra två datum i Haskell behöver vi först skapa två datumobjekt. Detta kan göras med hjälp av "Day" typen i Haskell som representerar ett datum.

```Haskell
import Data.Time

-- skapa två datumobjekt
let date1 = fromGregorian 2020 4 10
let date2 = fromGregorian 2020 4 20
```

För att jämföra dessa två datum, använder vi funktionen "compare" som finns inbyggd i Haskell. Den tar två argument och returnerar en "Ordering" typ som kan vara "LT" (mindre än), "GT" (större än) eller "EQ" (likvärdig).

```Haskell
-- jämför datumobjekten
let comparison = date1 `compare` date2

-- skriv ut resultatet
print comparison

-- output: LT
```

Nu kan vi använda detta resultat för att göra beslut baserat på jämförelsen av datum. Till exempel kan vi skriva en funktion som returnerar "True" om det första datumet är före det andra datumet och "False" annars.

```Haskell
-- funktion för att jämföra datum
compareDates :: Day -> Day -> Bool
compareDates date1 date2
    | date1 `compare` date2 == LT = True
    | otherwise = False

-- användning
compareDates date1 date2

-- output: True
```

## Djupdykning

I Haskell finns det några andra funktioner som kan användas för att jämföra datum på olika sätt. Till exempel kan vi använda "diffDays" för att få antalet dagar mellan två datum eller "addDays" för att lägga till ett visst antal dagar till ett datum.

```Haskell
-- diffDays
diffDays date1 date2

-- output: 10

-- addDays
addDays 5 date1

-- output: 2020-04-15 (Adderar 5 dagar till date1)
```

För mer information om dessa funktioner och hur man använder dem kan du konsultera Haskell dokumentationen.

## Se även

- [Haskell dokumentationen](https://www.haskell.org/documentation/)
- [Jämföring av datum i Haskell](https://www.tutorialspoint.com/compare-dates-in-haskell) (engelska)
- [Officiell webbplats för Haskell](https://www.haskell.org/)