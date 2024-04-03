---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:01:34.674924-07:00
description: "Hoe: Elm's kernfilosofie is Geen Runtime Uitzonderingen. Daarom maakt\
  \ Elm gebruik van zijn typesysteem met typen zoals `Maybe` en `Result` om fouten\
  \ te\u2026"
lastmod: '2024-03-13T22:44:50.732579-06:00'
model: gpt-4-0125-preview
summary: Elm's kernfilosofie is Geen Runtime Uitzonderingen.
title: Fouten afhandelen
weight: 16
---

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
