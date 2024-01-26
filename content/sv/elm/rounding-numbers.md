---
title:                "Avrundning av tal"
date:                  2024-01-26T03:45:31.616300-07:00
model:                 gpt-4-0125-preview
simple_title:         "Avrundning av tal"
programming_language: "Elm"
category:             "Elm"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/elm/rounding-numbers.md"
---

{{< edit_this_page >}}

## Vad & Varför?

Att avrunda tal innebär att justera ett decimaltal till dess närmaste hela värde eller till ett angivet antal decimaler. Programmerare avrundar för att minska komplexitet, förbättra läsbarhet eller matcha precisionkrav.

## Hur man gör:

Elms `Basics`-modul tillhandahåller finurliga funktioner för avrundning: `round`, `floor` och `ceiling`. Så här använder du dem.

```elm
import Basics exposing (round, floor, ceiling)

-- Avrunda till närmaste hela nummer
round 3.14    --> 3
round 3.5     --> 4

-- Avrunda nedåt
floor 3.999   --> 3

-- Avrunda uppåt
ceiling 3.001 --> 4

-- Avskär decimaler utan avrundning
truncate 3.76 --> 3
```

Elm tillhandahåller också `toLocaleString` för avrundning till ett fast antal decimaler:

```elm
import Float exposing (toLocaleString)

-- Avrunda till två decimaler
toLocaleString 2 3.14159 --> "3.14"
```

## Fördjupning

Elm är ett starkt typat funktionellt språk som delegerar sidoeffekter till arkitekturens "kanter". Detta betyder att funktioner som avrundning måste vara rena och förutsägbara. Historiskt sett är avrundning en vanlig operation i många programmeringsspråk som hanterar flyttalsaritmetikens oprecision.

Elms tillvägagångssätt för avrundning är rakt på sak – funktionerna är rena och följer matematiska definitioner för avrundning, uppåt- och nedåtrundning. Elm förutser vanliga behov genom att tillhandahålla inbyggda funktioner, eftersom precisionshantering ofta är ett krav, speciellt inom finans och grafik.

Alternativ till Elms inbyggda funktioner kan inkludera anpassade genomföranden med hjälp av aritmetiska operationer, men det lägger till onödig komplexitet när standardbiblioteket redan utför jobbet effektivt.

I den nuvarande versionen använder Elm JavaScripts underliggande flyttalsmatematik för dessa operationer, och håller sig därmed konsekvent med IEEE 754-standarden, vilket är något att komma ihåg när man överväger precision och potentiella flyttalsfel.

## Se även

- Elms officiella `Basics`-moduldokumentation: https://package.elm-lang.org/packages/elm/core/latest/Basics
- En detaljerad titt på hur flyttal fungerar i datorberäkningar: https://floating-point-gui.de/
- Elm `Float`-modulen för fler flyttalsoperationer: https://package.elm-lang.org/packages/elm/core/latest/Float