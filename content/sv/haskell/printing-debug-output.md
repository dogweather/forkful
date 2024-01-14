---
title:    "Haskell: Utskrift av felsökningsutdata"
keywords: ["Haskell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/sv/haskell/printing-debug-output.md"
---

{{< edit_this_page >}}

## Varför
Att skriva ut debug-utdata är ett viktigt verktyg för att felsöka och förstå vad som händer i ens program. Det kan hjälpa dig att hitta fel och förbättra prestandan på din kod.

## Hur man gör det
För att skriva ut debug-utdata i Haskell, använder man funktionen "trace" från "Debug.Trace" biblioteket. Detta gör det möjligt att skriva ut värden eller uttryck i din kod vid olika punkter i programmets körning.

```Haskell
import Debug.Trace

-- Exempel på användning
let x = 5
trace ("x är " ++ show x) x

-- Output: "x är 5"
```

## Djupdykning
När du använder "trace" funktionen är det viktigt att notera att det alltid kommer att returnera en IO-action, även om värdet du skriver ut inte är av typen IO. Detta är viktigt att komma ihåg för att undvika oönskade effekter i din kod.

Dessutom kan det vara bra att använda en funktion som "Debug.Trace.traceShow" istället för "trace" om du behöver skriva ut värden av typer som inte är instansierade med "show" klassen. Detta kommer att använda funktionen "Debug.Trace.traceShowId" istället för att bygga en "String" representation av värdet.

## Se även
- [Haskell - Debugging](https://www.haskell.org/c/documentation.html#debugging)
- [Debugging in Haskell](https://wiki.haskell.org/Debugging)
- [Debugging Purely Functional Code in Haskell](https://timothycrosley.github.io/what-is-functional-programming/exploring-side-effects.html#debugging)

## Källor
- [Debugging in Haskell](https://wiki.haskell.org/Debugging)