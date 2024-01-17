---
title:                "Utskrift av felsökningsutdata"
html_title:           "Haskell: Utskrift av felsökningsutdata"
simple_title:         "Utskrift av felsökningsutdata"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/haskell/printing-debug-output.md"
---

{{< edit_this_page >}}

# Vad & Varför?

Att skriva ut debug-utdata är när programmerare inkluderar extra information i koden för att hjälpa till att hitta och lösa buggar i sitt program. Detta innebär att man skriver ut variabler, värden och meddelanden i viktiga delar av koden för att kunna spåra problemen.

## Hur man gör det:

För att skriva ut debug-utdata i Haskell, kan du använda funktionen `trace` från paketet `Debug.Trace`. Här är ett exempel på hur det kan se ut:

```Haskell
import Debug.Trace

main :: IO ()
main = do
  let x = 5
  trace ("Value of x: " ++ show x) $ do
    let sum = x + 10
    putStrLn ("Sum: " ++ show sum)
```

Detta kommer att skriva ut följande:

```
Value of x: 5
Sum: 15
```

Som du kan se, skriver vi ut värdet av variabeln `x` före vi beräknar summan. På så sätt kan vi kontrollera att värdet är korrekt.

## Djupdykning:

Att skriva ut debug-utdata är ett vanligt verktyg som används av programmerare för att felsöka och förbättra sina program. Det är särskilt användbart när man arbetar med större program och komplexa kodsegment.

Ett alternativ till att skriva ut debug-utdata är att använda en speciell debugging-miljö eller en interaktiv debugger. Dessa verktyg erbjuder en mer omfattande och interaktiv metod för att felsöka koden.

För implementationen av `trace` använder Haskell en funktionell teknik som kallas "monad transformers". Detta är en avancerad teknik som gör det möjligt att skapa användbara funktioner som `trace` för att hantera debugging-utdata på ett enklare sätt.

## Se även:

- [Debug.Trace dokumentation](http://hackage.haskell.org/package/base-4.15.0.0/docs/Debug-Trace.html)
- [Haskell debugging verktyg och tekniker](https://wiki.haskell.org/Debugging)