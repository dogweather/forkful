---
title:                "Analysera ett datum från en sträng"
html_title:           "Kotlin: Analysera ett datum från en sträng"
simple_title:         "Analysera ett datum från en sträng"
programming_language: "Elm"
category:             "Elm"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/elm/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att tolka ett datum från en sträng innebär att omvandla en textrepresentation av ett datum till ett faktiskt datumvärde som datorn kan förstå och hantera. Utvecklare gör detta för att enkelt kunna behandla och manipulera datumdata som kommer i strängformat, vilket är vanligt i olika datakällor och API:er.

## Hur man gör:
Här är ett exempel på hur du tolkar ett datum från en sträng i Elm:
```Elm
import Date
import Time
import Time.Extra exposing (Hour, Minute, Second)

parseDate : String -> Maybe Date
parseDate str =
    Time.fromString str
        |> Maybe.andThen Time.toTime
        |> Maybe.map Date.fromTime

-- usage
-- parseDate "2021-03-14T15:09:26.535Z" --> Just (Date.fromTime { year = 2021, month = Mar, day = 14 })

```
Koden ovan skapar en funktion `parseDate` som tar en sträng och försöker konvertera den till ett `Maybe Date`-objekt.

## Djupdykning
Historiskt sett har datumtolkning varit en utmaning för utvecklare, särskilt på grund av olika datumformat och tidszoner. I Elm är standardbiblioteket för tid och datum `Time`, som har funktioner för att både tolka och skapa strängrepresentationer av tider och datum.

En alternativ funktion för att tolka datum i formatet "åååå-mm-dd" från en sträng i Elm är `Date.fromIsoString`. Observera dock att denna funktion bara accepterar datum i ISO 8601-format.

När det gäller implementeringsdetaljer i `Time.fromString`, använder den JavaScripts `Date.parse`-funktion under huven, vilket innebär att den accepterar datumsträngar i samma format som `Date.parse`.

## Se även
För mer information om hur du arbetar med datum och tid i Elm, se följande länkar:
- Elm Time-biblioteket: https://package.elm-lang.org/packages/elm/time/latest/
- Elm Date-biblioteket: https://package.elm-lang.org/packages/rtfeldman/elm-iso8601-date-strings/latest/
- Officiell Elm guide: https://guide.elm-lang.org/