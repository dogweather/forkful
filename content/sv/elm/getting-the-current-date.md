---
title:                "Elm: Att få dagens datum"
simple_title:         "Att få dagens datum"
programming_language: "Elm"
category:             "Elm"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/elm/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Varför

Att kunna få den aktuella datumen är en viktig del av Elm programmering eftersom det ger en uppdaterad information till användare och tillåter applikationer att fungera korrekt baserat på tidsberoende funktioner.

## Hur man gör

För att få den aktuella datumen i Elm, måste man först importera `Time` modulen. Sedan kan man använda `Time.now` funktionen för att få den aktuella datumen som en `Posix` värdering.

```Elm
import Time

currentDate : Time.Posix
currentDate =
  Time.now
```

För att sedan konvertera den värderingen till ett mer läsbart format, såsom dag, månad och år, kan man använda funktionen `Time.toDayDate` och ge den `currentDate` som ett argument.

```Elm
currentDayDate : Maybe (Int, Int, Int)
currentDayDate =
  Time.toDayDate currentDate
```

Outputen av `currentDayDate` skulle vara `Just (30, 11, 2021)`.

## Djupdykning

`Time` modulen i Elm tillåter också hantering av tidszoner och formatering av datum för mer specifika behov. Genom att använda funktionen `Time.fromPosix` kan man få en `Time.Zone` värdering som kan användas för att anpassa datumen till en specifik tidszon.

```Elm
import Time exposing (Zone, ZonedDateTime, fromPosix)

currentTime : Time.ZonedDateTime
currentTime =
  fromPosix (Time.now) (Time.utc)

currentDate : String
currentDate =
  Time.format "%Y-%m-%d" currentTime (Time.fromZone (Zone.name currentTime))
```

Outputen av `currentDate` skulle vara i formatet `2021-11-30` och anpassad till den globala tidszonen.

## Se också

- [Elm Time modulen dokumentation](https://package.elm-lang.org/packages/elm/time/latest/)
- [Officiell Elm språk guide](https://guide.elm-lang.org/)
- [Hur man hanterar tidszoner i Elm](https://medium.com/@ryuclarke/working-with-time-zones-in-elm-c7367f71bfb0)