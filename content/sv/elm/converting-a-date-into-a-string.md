---
title:                "Omvandla ett datum till en sträng"
html_title:           "C#: Omvandla ett datum till en sträng"
simple_title:         "Omvandla ett datum till en sträng"
programming_language: "Elm"
category:             "Elm"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/elm/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Vad & Varför?

Att konvertera ett datum till en sträng i programmering innebär att omvandla det numeriska värdet av ett datum till en läsbar tekststräng. Programmerare gör detta för att förbättra användarupplevelsen och presentera information på ett mer begripligt sätt.

## Så här gör du:

I Elm, vi kan använda `Date.toIsoString` funktion att konvertera datum till sträng. Vi kan också använda `Date.fromTime` för att skapa datum från tidsstämpel. Här är ett exempel:

```Elm
import Date exposing (Date)
import Time exposing (Posix)

toIsoStringExample : Posix -> String
toIsoStringExample time =
    time
        |> Date.fromTime
        |> Date.toIsoString
```

Om vi till exempel kör `toIsoStringExample` med värde `1590497541000`, får vi `2020-05-26`.

## Fördjupning

I tidiga versioner av Elm, sättet att behandla datum var knepigt eftersom det inte fanns någon inbyggd funktion för detta. Med introduktionen av `Date.toIsoString` i Elm 0.19, blev det mycket enklare.

Alternativt kan programmerare också använda `DateFormat` bibliotek för mer avancerade formateringar, till exempel att ange datumets format själv.

En sak att notera med Elm är att den behandlar datum konvertering på ett mycket strikt sätt. Detta innebär att det är säkert från vanliga buggar och problem som kan uppstå i andra språk när man arbetar med datum och tid.

## Se också

Om du vill veta mer om datumkonvertering och hantering i Elm, kolla in dessa länkar:

- [Elm Date Library Documentation](https://package.elm-lang.org/packages/elm/time/latest)
- [DateFormat Library](https://package.elm-lang.org/packages/rtfeldman/elm-iso8601-date-strings/latest)