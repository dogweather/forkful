---
title:                "Fouten afhandelen"
aliases:
- /nl/elm/handling-errors/
date:                  2024-01-28T22:01:34.674924-07:00
model:                 gpt-4-0125-preview
simple_title:         "Fouten afhandelen"

tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/elm/handling-errors.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?
Foutafhandeling betekent het schrijven van code die kan anticiperen op en omgaan met dingen die misgaan. Programmeurs doen dit om crashes te voorkomen, de integriteit van gegevens te beschermen en gebruikers te voorzien van gracieuze terugvalopties.

## Hoe:
Elm's kernfilosofie is Geen Runtime Uitzonderingen. Daarom maakt Elm gebruik van zijn typesysteem met typen zoals `Maybe` en `Result` om fouten te behandelen.

Voor het `Maybe` scenario:

```Elm
safeDivide : Float -> Float -> Maybe Float
safeDivide numerator denominator =
    if denominator == 0 then
        Nothing
    else
        Just (numerator / denominator)
        
-- Wanneer je het uitvoert:

safeDivide 10 2
--> Just 5

safeDivide 10 0
--> Nothing
```

Voor het `Result` scenario:

```Elm
type Error = DivisionByZero

safeDivide : Float -> Float -> Result Error Float
safeDivide numerator denominator =
    if denominator == 0 then
        Err DivisionByZero
    else
        Ok (numerator / denominator)

-- En het gebruiken:

safeDivide 10 2
--> Ok 5

safeDivide 10 0
--> Err DivisionByZero
```

## Diepere Duik
Elm's typesysteem is strikt, wat helpt om fouten vroeg te vangen. Historisch gezien leunden de meeste talen op uitzonderingen en runtime checks, maar Elm koos voor compile-time garanties. Alternatieven zoals `Result` bieden gedetailleerde foutinformatie, terwijl `Maybe` eenvoudiger is voor ja-nee scenario's. Elm's foutafhandeling moedigt ontwikkelaars aan om upfront alle paden te overwegen, waardoor de valkuilen van vergeten foutgevallen worden vermeden.

## Zie Ook:
- Elm’s officiële gidssectie over foutafhandeling: [Foutafhandeling – Een Introductie](https://guide.elm-lang.org/error_handling/)
- Elm `Maybe` documentatie: [Elm – Maybe](https://package.elm-lang.org/packages/elm/core/latest/Maybe)
- Elm `Result` documentatie: [Elm – Result](https://package.elm-lang.org/packages/elm/core/latest/Result)
