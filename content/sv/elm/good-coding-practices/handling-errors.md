---
date: 2024-01-26 00:51:36.490459-07:00
description: "Hur man g\xF6r: Elms k\xE4rnfilosofi \xE4r Inga K\xF6rtidsundantag.\
  \ S\xE5, Elm utnyttjar sitt typsystem med typer som `Maybe` och `Result` f\xF6r\
  \ att hantera fel. F\xF6r\u2026"
lastmod: '2024-03-13T22:44:37.836958-06:00'
model: gpt-4-1106-preview
summary: "Elms k\xE4rnfilosofi \xE4r Inga K\xF6rtidsundantag."
title: Hantering av fel
weight: 16
---

## Hur man gör:
Elms kärnfilosofi är Inga Körtidsundantag. Så, Elm utnyttjar sitt typsystem med typer som `Maybe` och `Result` för att hantera fel.

För `Maybe`-scenariot:

```Elm
safeDivide : Float -> Float -> Maybe Float
safeDivide numerator denominator =
    if denominator == 0 then
        Nothing
    else
        Just (numerator / denominator)
        
-- När du kör det:

safeDivide 10 2
--> Just 5

safeDivide 10 0
--> Nothing
```

För `Result`-scenariot:

```Elm
type Error = DivisionByZero

safeDivide : Float -> Float -> Result Error Float
safeDivide numerator denominator =
    if denominator == 0 then
        Err DivisionByZero
    else
        Ok (numerator / denominator)

-- Och använda det:

safeDivide 10 2
--> Ok 5

safeDivide 10 0
--> Err DivisionByZero
```

## Fördjupning
Elms typsystem är strikt, vilket hjälper till att upptäcka fel tidigt. Historiskt har de flesta språk förlitat sig på undantag och körtidskontroller, men Elm valde garantier vid kompileringstid. Alternativ som `Result` tillåter detaljerad felinformation, medan `Maybe` är enklare för ja-nej-scenarier. Elms felhantering uppmuntrar utvecklare att överväga alla vägar i förväg, och undviker fallgropar av glömda felfall.

## Se även:
- Elm:s officiella guideavsnitt om felhantering: [Felhantering – En introduktion](https://guide.elm-lang.org/error_handling/)
- Elm `Maybe`-dokumentation: [Elm – Maybe](https://package.elm-lang.org/packages/elm/core/latest/Maybe)
- Elm `Result`-dokumentation: [Elm – Result](https://package.elm-lang.org/packages/elm/core/latest/Result)
