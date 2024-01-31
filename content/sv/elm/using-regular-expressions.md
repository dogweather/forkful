---
title:                "Använda reguljära uttryck"
date:                  2024-01-19
html_title:           "Bash: Använda reguljära uttryck"
simple_title:         "Använda reguljära uttryck"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/elm/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Reguljära uttryck är mönster för strängmatchning. Programmerare använder dem för att söka, ersätta och validera textdata snabbt.

## Steg för steg:
Elm erbjuder inte inbyggt stöd för reguljära uttryck, men vi kan använda `elm/regex` paketet. Här är ett enkelt exempel:

```Elm
import Regex exposing (Regex, fromString, find, Match)

-- Skapar ett reguljärt uttryck
maybeRegex : Maybe Regex
maybeRegex = fromString "h[aeiou]llo"

-- Använder det reguljära uttrycket för att hitta matcher
findMatches : String -> List Match
findMatches input =
    case maybeRegex of
        Just regex ->
            find regex input
        Nothing ->
            []

-- Visa ett exempel
sampleOutput : List Match
sampleOutput = findMatches "hello hallo hillo"
```

Kör koden ovan och `sampleOutput` blir en lista av `Match` objekt där varje `Match` innehåller detaljer om varje matchning.

## Djupdykning:
Reguljära uttryck, ofta förkortade som "regex", används sedan 1950-talet. I Elm hanteras de lite annorlunda eftersom Elm fokuserar på säkerhet och förutsägbarhet. Istället för inbyggt stöd som i JavaScript, tillhandahåller Elm ett paket `elm/regex`. Detta paket anser Elm's typsystem och därmed minskar riskerna för oförutsedda runtime-fel. För textmanipulering utan regex kan man använda inbyggda strängfunktioner som `String.contains`, `String.split` och `String.join`.

## Se även:
- Elm Regex paketet: [http://package.elm-lang.org/packages/elm/regex/latest](http://package.elm-lang.org/packages/elm/regex/latest)
- Elm strängdokumentation: [https://package.elm-lang.org/packages/elm/core/latest/String](https://package.elm-lang.org/packages/elm/core/latest/String)
