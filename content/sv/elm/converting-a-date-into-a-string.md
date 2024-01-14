---
title:                "Elm: Omvandla ett datum till en sträng"
programming_language: "Elm"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/elm/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Varför

Att konvertera ett datum till en sträng kan vara en viktig del av att skriva funktionell kod i Elm. Med detta kan man enkelt visa datum i ett visst format eller omvandla dem för att passa olika språk eller regioner. Det kan även hjälpa till att samordna data med andra system.

## Så här gör du

För att konvertera ett datum till en sträng använder man sig av funktionen Date.toString. Med detta kan man specificera vilket format man vill ha på datumet samt det aktuella datumet man vill konvertera. Nedan följer ett exempel på hur man kan använda denna funktion:

```Elm
Date.fromString "2019-07-17" |> Maybe.withDefault Date.today
|> Date.toString {day = "/" , month = "/"}
```

Output från detta exempel kommer att bli "17/07".

## Djupdykning

Funktionen Date.toString har flera olika argument som man kan använda för att anpassa formatet på strängen. Det finns ett antal fördefinierade format såsom standarformatet för datum ("YY/MM/DD") eller även specifika format för t.ex. tider, årtal eller veckodagar. Man kan även skapa egna anpassade format genom att lägga till specifika symboler för år, månad och dag. Man kan också använda sig av flags för att anpassa strängen baserat på användarens språk och region.

## Se även

- [Elm Date paket](https://package.elm-lang.org/packages/elm/time/latest/Date)
- [Elm Date dokumentation](https://package.elm-lang.org/packages/elm/time/latest/Time# Date)