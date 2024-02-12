---
title:                "Skriva ut felsökningsdata"
aliases:
- /sv/haskell/printing-debug-output.md
date:                  2024-01-20T17:52:59.681210-07:00
model:                 gpt-4-1106-preview
simple_title:         "Skriva ut felsökningsdata"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/haskell/printing-debug-output.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Debug-utskrifter hjälper programmerare att förstå vad deras kod gör genom att visa mellanliggande värden och processflöden. De används för att snabbt lokalisera och rätta till fel i koden.

## How to:
Att skriva ut debug-meddelanden i Haskell kan göras med `print` eller `putStrLn`. Här kommer några exempel:

```Haskell
main :: IO ()
main = do
  putStrLn "This is a debug message"

  let number = 42
  print number  -- print kan hantera alla typer som är en instans av Show
```
Kör du detta får du följande utskrift:

```
This is a debug message
42
```

## Deep Dive
I Haskell's tidiga dagar var I/O, inklusive enkel utskrift, omständlig på grund av språkets rena funktionsnatur. Nu har vi `IO` monaden som hanterar effekter. För debug-ändamål kan `Debug.Trace` också användas, men den bör undvikas i produktionskod eftersom den får sidoeffekter i vad som ser ut att vara ren kod. Vi har också `printf` från `Text.Printf` för formaterad text, som är lik `printf` i C.

```Haskell
import Debug.Trace (trace)

traceExample :: Int -> Int -> Int
traceExample x y = trace ("Adding " ++ show x ++ " and " ++ show y) (x + y)
```

Användning av `trace` ska dock användas med försiktighet då det kan orsaka svåråtkomliga buggar. Andra verktyg för debugging, som GHCi's debugger och mer specialiserade bibliotek finns, som kan vara lämpligare för avancerade behov.

## See Also
- The GHC User's Guide on debugging: https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/debugging.html
- `Debug.Trace` module: http://hackage.haskell.org/package/base-4.16.0.0/docs/Debug-Trace.html
- `Text.Printf` module: http://hackage.haskell.org/package/base-4.16.0.0/docs/Text-Printf.html
