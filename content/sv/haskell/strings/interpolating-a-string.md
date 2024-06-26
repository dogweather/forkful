---
date: 2024-01-20 17:50:57.947780-07:00
description: "Hur man g\xF6r: I Haskell hanterar vi inte stringinterpolation ur l\xE5\
  dan. Men med biblioteket `Data.Text`, och Template Haskell-funktionen `printf` fr\xE5\
  n\u2026"
lastmod: '2024-03-13T22:44:37.941731-06:00'
model: gpt-4-1106-preview
summary: "I Haskell hanterar vi inte stringinterpolation ur l\xE5dan."
title: "Interpolera en str\xE4ng"
weight: 8
---

## Hur man gör:
I Haskell hanterar vi inte stringinterpolation ur lådan. Men med biblioteket `Data.Text`, och Template Haskell-funktionen `printf` från `Text.Printf`, är det enkelt. Här är ett exempel:

```haskell
{-# LANGUAGE OverloadedStrings #-}

import Text.Printf (printf)
import Data.Text (pack, unpack, Text)

main :: IO ()
main = do
  let name = "Elsa"
  let age = 26
  putStrLn $ unpack (interpolate name age)

interpolate :: String -> Int -> Text
interpolate name age = pack (printf "Hej, jag heter %s och jag är %d år gammal." name age)

-- Output
-- Hej, jag heter Elsa och jag är 26 år gammal.
```

## Djupdykning
Stringinterpolering som koncept har funnits länge i programmering, ofta inbyggt i språk som Python och JavaScript. Haskell, som är starkt typat och fokuserar på funktionell renhet, har inte inbyggd interpolering. Det beror på att funktioner som `printf` kan vara otydliga och potentiellt osäkra.

Men med paket som `Data.Text` och Template Haskell kan vi lika enkelt interpolera strängar. `Text.Printf`-modulen är inspirerad av C's `printf` och tillhandahåller en typsäker variant av C's funktion.

En annan metod för stringinterpolering i Haskell är genom `interpolate` biblioteket, som erbjuder en mer typsäker och uttrycksfull syntax. Till exempel kan du använda `[i|...|]` quasi-litteraler för att interpolera:

```haskell
{-# LANGUAGE QuasiQuotes #-}

import Data.String.Interpolate (i)

name = "Olle"
age = 30

main :: IO ()
main = putStrLn [i|Hej, jag heter #{name} och jag är #{age} år gammal.|]

-- Output
-- Hej, jag heter Olle och jag är 30 år gammal.
```

Kom ihåg, när du använder interpolering, att hålla ett öga på prestanda, eftersom det kan vara mer kostsamt än att slå samman strängar (concatenation).

## Se även
- Haskells `Data.Text` dokumentation: https://hackage.haskell.org/package/text
- `Text.Printf` dokumentation: https://hackage.haskell.org/package/base-4.14.0.0/docs/Text-Printf.html
- `interpolate` bibliotek: https://hackage.haskell.org/package/interpolate
- Haskell Wiki om Stringinterpolering: https://wiki.haskell.org/Interpolation
