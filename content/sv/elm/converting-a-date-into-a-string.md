---
title:                "Omvandla ett datum till en sträng"
html_title:           "Elm: Omvandla ett datum till en sträng"
simple_title:         "Omvandla ett datum till en sträng"
programming_language: "Elm"
category:             "Elm"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/elm/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att konvertera ett datum till en sträng betyder att man omvandlar ett datum format (t.ex. 2021-05-12) till en text (t.ex. "12 maj 2021"). Programerare gör detta för att kunna visa datumet i ett mer läsbart format för användaren.

## Hur:
Det är enkelt att konvertera ett datum till en sträng i Elm. Använd funktionen `Date.toDayString` för att få datumet i formatet "dd mm yyyy". Om du vill ha en specifik månadsnamn så kan du använda funktionen `Date.toFormattedString` tillsammans med en viss formatteringssträng. Se exempel nedan.

```Elm
import Date exposing (Date)
import Time exposing (hour, minute, second)

-- Konvertera datumet till "dd mm yyyy"
Date.toDayString <|
  Date.fromTime 0
  |> Date.month 5
  |> Date.day 12
  |> Date.year 2021
--> "12 maj 2021"

-- Konvertera datumet till en annan format
Date.toFormattedString "%d. %B %Y" <|
  Date.fromTime 0
  |> Date.month 5
  |> Date.day 12
  |> Date.year 2021
--> "12 maj 2021"
```

## Djupdykning:
Att kunna konvertera datum till strängar är ett vanligt behov inom programmering. Det är särskilt användbart när man ska visa datum i en användargränssnitt. Innan så var det vanligt att man var tvungen att skriva egna funktioner för datumkonvertering, men tack vare funktioner som `Date.toDayString` är det nu enklare och effektivare att göra detta i Elm.

En alternativ metod för att konvertera datum till strängar är att använda funktionen `Time.toString` tillsammans med `Time.now` för att få datumet och sedan använda `String.split` för att dela upp datumet i sina olika delar och sedan sätta ihop dem i ett önskat format.

För de som är intresserade av implementationen bakom funktionen `Date.toDayString` så är den baserad på ISO 8601-standard och använder sig av `Date.toString` för att få datumet i formatet "yyyy-mm-dd" innan det sedan delas upp och sätts ihop enligt ett visst format.

## Se även:
- Officiell dokumentation för Date-modulen: https://package.elm-lang.org/packages/elm/time/latest/Time-Date
- ISO 8601-standard: https://sv.wikipedia.org/wiki/ISO_8601
- Elm's inbyggda funktioner för datumkonvertering: https://package.elm-lang.org/packages/elm/time/latest/