---
title:                "Beräkning av ett datum i framtiden eller förflutna"
html_title:           "Elm: Beräkning av ett datum i framtiden eller förflutna"
simple_title:         "Beräkning av ett datum i framtiden eller förflutna"
programming_language: "Elm"
category:             "Elm"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/elm/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

# Vad & Varför?
Beräkna ett framtida eller tidigare datum handlar om att justera dagar, månader eller år från ett specifikt datum. Programmerare gör detta för att hantera tidbaserade händelser som schemaläggning, påminnelser och tidsramar.

# Hur man gör:
```Elm
import Date exposing (..)
import Time exposing (millisecond, Minute, second)

calculateFutureDate : Date.Date -> Int -> Date.Date
calculateFutureDate today daysToAdd =
    Date.add Days daysToAdd today

calculatePastDate : Date.Date -> Int -> Date.Date
calculatePastDate today daysToSubtract =
    Date.add Days (-daysToSubtract) today
```
Kodexemplet ovan visar hur man kan skapa funktioner för att beräkna framtida eller tidigare datum baserat på ett antal dagar.

# Djupdyk:
Historiskt sett har datumberäkningar varit en utmaning på grund av kalendersystemens komplexitet och variation. I Elm erbjuder `Date`-modulen enkelhet vid hantering av datum och tider. Ett alternativ till `Date` är `Time`-modulen, som tillhandahåller mer detaljerade tidsenheter som timmar, minuter och sekunder. Detaljerna i implementeringen av datumberäkningar i Elm bygger på att dagar läggs till eller dras från dagens datum, vilket sedan konverterar till rätt datum.

# Se också:
1. [Elm Official Guide](https://guide.elm-lang.org/)
2. [Elm Date Documentation](https://package.elm-lang.org/packages/elm/time/latest/Date)
3. [Elm Time Documentation](https://package.elm-lang.org/packages/elm/time/latest/Time)