---
title:                "Att tolka ett datum från en sträng"
html_title:           "Elm: Att tolka ett datum från en sträng"
simple_title:         "Att tolka ett datum från en sträng"
programming_language: "Elm"
category:             "Elm"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/elm/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

# Vad & Varför?
Att tolka ett datum från en sträng är en process där man konverterar en textbaserad representation av ett datum till ett datumobjekt som kan hanteras av ditt program. Detta är användbart när man till exempel vill hämta och visa datum från en databas eller användarens inmatning. Det sparar programmerare tid och minskar risken för felaktiga inmatningar.

# Så här gör du:
```Elm
import Date exposing (Date)
import Date.Format exposing (parse)

parse "2021-10-25" "yyyy-MM-dd"
--> Ok (Date.fromCalendarDate 2021 10 25)

parse "/ 10/25/2021" "MM/dd/yyyy"
--> Ok (Date.fromCalendarDate 2021 10 25)

parse "Okt 25, 2021" "MMM dd, yyyy"
--> Ok (Date.fromCalendarDate 2021 10 25)
```
## Djupdykning
Att tolka datum från en sträng är en vanlig uppgift för programmerare inom olika språk och plattformar. Det finns flera olika sätt att utföra detta i Elm, inklusive inbyggda funktioner och tredjepartsbibliotek. Det är viktigt att tänka på eventuella regionala skillnader i datumformat och att hantera felaktiga inmatningar för att undvika programfel.

## Se även
[Elm Date documentation](https://package.elm-lang.org/packages/elm/time/latest/Date)