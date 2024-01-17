---
title:                "Beräkna ett datum i framtiden eller det förflutna"
html_title:           "Elm: Beräkna ett datum i framtiden eller det förflutna"
simple_title:         "Beräkna ett datum i framtiden eller det förflutna"
programming_language: "Elm"
category:             "Elm"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/elm/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att beräkna ett datum i framtiden eller förflutet är när man använder kod för att förutsäga en specifik dag, månad eller år baserat på ett startdatum och ett antal dagar som ska läggas till eller dras bort. Detta kan vara användbart för att skapa kalendrar, planera händelser eller visualisera tidsperioder. Programmerare använder ofta denna teknik för att göra sina appar och webbplatser mer dynamiska och användarvänliga.

## Hur man gör:
För att beräkna ett datum i Elm kan du använda Date.add-funktionen. Till exempel, för att få det datum som är 30 dagar efter ett givet datum:
```Elm
import Date exposing (..)

futureDate : Date
futureDate = 
    add 30 Days myDate
```
För att få det datum som är 30 dagar före ett givet datum:
```Elm
import Date exposing (..)

pastDate : Date
pastDate = 
    subtract 30 Days myDate
```
Se till att importera Date-modulen och ersätta "myDate" med ditt eget startdatum.

## Djupdykning:
Beräkning av datum i framtiden eller förflutet har använts av människor sedan antiken. Tidiga civilisationer som egyptierna och babylonierna använde solkalendern för att förutsäga datum och händelser baserat på solens position. Idag finns det också andra programmeringsspråk som erbjuder liknande funktioner som Elm, till exempel JavaScripts "Date()" -funktion.

En viktig detalj att tänka på är att datum och tidshantering kan vara komplicerat på grund av skillnaderna mellan tidszoner, sommar- och vintertid, och kalenderformat. Därför kan det vara användbart att använda bibliotek som "elm-time-extra" för att hantera dessa utmaningar.

## Se även:
Här är några resurser som kan hjälpa dig att lära dig mer om att beräkna datum i Elm:
- Officiell Elm dokumentation för Date-modulen: https://package.elm-lang.org/packages/elm/time/latest/Date
- Officiella Elm-gemenskapspaketet för att hantera datum och tid: https://package.elm-lang.org/packages/elm-community/elm-time-extra/latest/