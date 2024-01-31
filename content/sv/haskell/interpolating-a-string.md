---
title:                "Interpolera en sträng"
date:                  2024-01-20T17:50:57.947780-07:00
model:                 gpt-4-1106-preview
simple_title:         "Interpolera en sträng"

category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/haskell/interpolating-a-string.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Stringinterpolering låter dig spränga in variabler direkt i en sträng. Programmerare använder det för att skapa dynamisk text snabbt och smidigt.

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
