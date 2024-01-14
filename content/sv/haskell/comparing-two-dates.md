---
title:                "Haskell: Jämföra två datum"
simple_title:         "Jämföra två datum"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/haskell/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Varför

Att jämföra två datum är en viktig del av många programmeringsprojekt. Det kan hjälpa till med att sortera eller filtrera data, beräkna tidsintervaller eller hantera tidslinjer. Att kunna göra detta på ett effektivt och pålitligt sätt är avgörande för många program.

## Så här gör du

För att jämföra två datum i Haskell, måste vi först importera Date.Time-modulen. Sedan kan vi använda funktioner som `diffDays` och `addDays` för att hantera datum på en enkel nivå.

```Haskell
import Data.Time

-- Skapa två datum
let date1 = fromGregorian 2020 1 1
let date2 = fromGregorian 2020 2 1

-- Jämför dagar mellan två datum
let daysBetween = diffDays date2 date1
-- Resultatet blir 31

-- Lägg till 10 dagar till ett datum
let laterDate = addDays 10 date1
-- Resultatet blir 2020-01-11
```

Haskell erbjuder också mer avancerade sätt att hantera datum, som att konvertera dem till olika tidszoner och formatera dem på specifika sätt. Det finns också många bibliotek som kan hjälpa till med mer komplicerade jämförelser, beräkningar och manipulationer av datum.

## Fördjupa dig

Att förstå hur datum fungerar och hur man kan hantera dem är ett viktigt steg mot att bli en bättre Haskell-programmerare. Det finns många olika sätt att representera datum och hur man jämför dem kan variera beroende på ens behov. Det är också viktigt att förstå skillnaden mellan lokala och globala tidszoner, och hur man hanterar dessa i sina program.

## Se även

- [Date.Time-modulen i Haskell-dokumentationen](https://hackage.haskell.org/package/time-1.9.3/docs/Data-Time.html)
- [Hoogle, ett sökmotor för Haskell-funktioner](https://www.haskell.org/hoogle/)