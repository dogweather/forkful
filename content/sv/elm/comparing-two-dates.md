---
title:                "Elm: Jämförande av två datum"
programming_language: "Elm"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/elm/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Varför

Att jämföra två datum kan vara väldigt användbart i många olika programmeringssammanhang. Oavsett om du behöver kontrollera om ett datum har passerat, räkna ut hur många dagar som gått mellan två datum eller helt enkelt sortera datum i en lista, är det en viktig kunskap för utvecklare. I denna bloggpost kommer vi att gå igenom hur man jämför två datum i Elm.

## Hur man gör

För att jämföra två datum i Elm kan man använda funktionen `compare` tillsammans med modulen `Date`. Här är ett exempel:

```Elm
import Date exposing (..)

myDate1 = fromString "2020-12-01"
myDate2 = fromString "2020-12-05"

result = compare myDate1 myDate2 
```

I detta exempel jämför vi två olika datum, "2020-12-01" och "2020-12-05". Funktionen `fromString` används för att konvertera strängarna till datumobjekt och sedan används `compare` för att jämföra dem. Resultatet är en `Order` som kan vara antingen `LT` (less than), `GT` (greater than) eller `EQ` (equal).

En annan funktion som kan vara användbar är `daysBetween` som räknar ut antalet dagar mellan två datum:

```Elm
import Date exposing (..)

myDate1 = fromString "2020-12-01"
myDate2 = fromString "2020-12-05"

result = daysBetween myDate1 myDate2 
```

I detta exempel skulle resultatet bli `4`, eftersom det är fyra dagar mellan de två datumobjekten.

## Djupdykning

När man jämför datum i Elm är det viktigt att förstå hur de representeras. Date-objekt består av flera olika delar såsom år, månad, dag, timme, minut och sekund. När man jämför två datum är det viktigt att man också tar hänsyn till alla dessa delar, inte bara datumet.

Det finns också flera olika funktioner för att skapa och manipulera datum i Elm, som `add` för att lägga till en viss tidsperiod till ett datum och `startOfYear` för att få det första datumet av ett visst år.

Det är också viktigt att vara medveten om att jämföra datum kan bli problematiskt om man inte hanterar tidszoner på rätt sätt. Det är alltid rekommenderat att konvertera till UTC-tid innan man jämför datumobjekt.

## Se även

- Officiell Elm Date-dokumentation: https://package.elm-lang.org/packages/elm/time/latest/Date
- En tutorial om att arbeta med datum och tid i Elm: https://elmprogramming.com/dates-and-times.html
- Elm Date-paketet på GitHub: https://github.com/elm-lang/elm-package/tree/master/packages/elm/time/latest