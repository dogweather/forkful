---
title:                "Elm: Hämta aktuellt datum"
programming_language: "Elm"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/elm/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Varför

Att få den aktuella datumet är en vanlig uppgift i många program. Det kan användas för att visa tidsstämplar på inlägg, skapa kalendrar, eller helt enkelt för att hålla koll på dagens datum. I denna bloggpost kommer vi att titta på hur man enkelt kan få den aktuella datumet i Elm.

## Så här gör du

För att få den aktuella datumet i Elm använder vi funktionen `Date.now`, som returnerar ett datumobjekt. Vi kan sedan använda den inbyggda funktionen `toWeekday` för att få veckodagen och `toDay` för att få den aktuella dagen på månaden.

```Elm
import Time exposing (..)

date : Time.Posix
date =
    Date.now

weekday : Number
weekday =
    Date.toWeekday date

day : Number
day =
    Date.toDay date
```

Om vi skulle köra detta skulle vi få en output som liknar detta:

```Elm
date: Time.Posix 1598774829847
weekday: 3
day: 29
```

Som du kan se är `date` en Posix-värde, vilket är en vanlig representation av datum och tid i många programmeringsspråk. Vi kan använda den inbyggda funktionen `toString` för att konvertera det till ett mer läsbart format.

```Elm
import Time exposing (..)

date : String
date =
    Date.toString date
```

Outputen skulle då se ut på följande sätt:

```Elm
date: String "Thu Aug 06 2020 22:13:49 GMT+0200 (Central European Standard Time)"
```

## Djupdykning

En viktig sak att tänka på när man hanterar datum i Elm är att det stöder Internationalization and Localization (I18n). Det innebär att vi kan använda funktioner som `toStringWith`, `monthToString` och `dayOfWeekToString` för att konvertera datum- och tidsinformation till olika språk och format.

För att fördjupa oss ännu mer kan vi också utforska den inbyggda modulen `Time` som ger oss fler funktioner för att hantera datum och tid, som `nowInUtc` för att få den aktuella tiden i UTC och `fromCalendarDate` för att skapa ett datumobjekt baserat på år, månad och dag.

## Se även

- Elm's officiella dokumentation om Date: https://package.elm-lang.org/packages/elm/time/latest/Time#Date
- Användning av Datum och Tid i Elm: https://www.elm-tutorial.org/en/04-managing-real-application/02-date-time.html 
- Elm Spelprogrammering: Problemet med datum och tid: https://thoughtbot.com/blog/elm-gamedev-the-problem-with-time
- Elm's dokumentation om Internationalization and Localization: https://package.elm-lang.org/packages/elm-lang/intl/latest/