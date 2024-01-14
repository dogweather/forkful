---
title:                "Haskell: Utmatning av felsökningsresultat"
programming_language: "Haskell"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/haskell/printing-debug-output.md"
---

{{< edit_this_page >}}

## Varför

Det finns många olika anledningar till varför programmerare skulle vilja använda debugging-utskrifter i Haskell. Det kan vara ett användbart verktyg för att förstå vad som händer i koden, identifiera fel och effektivisera utvecklingsprocessen.

## Hur man gör

För att skriva ut debug-utskrifter i Haskell kan vi använda oss av `Debug.Trace` modulen. Vi importerar modulen och använder funktionen `trace` som tar in en sträng som argument för vad som ska skrivas ut. Vi kan även använda oss av funktionen `traceShow` för att skriva ut det exakta värdet av en variabel.

```
Haskell
import Debug.Trace

main :: IO ()
main = do
  let x = 5
  trace "Värdet av x är: " $ traceShow x
```
Output:
```
Värdet av x är: 
5
```

## Djupdykning

Det finns ett par saker att tänka på när det kommer till debugging-utskrifter i Haskell. För det första, se till att bara använda dem under utvecklingsfasen och ta bort dem innan du pushar koden till produktion. Det kan också vara användbart att inkludera en no-op-funktion för debug-utskrifter så att de inte påverkar prestandan i produktion.

En annan sak att tänka på är att `trace` och `traceShow` endast fungerar i `IO` monaden, så de kan inte användas direkt i `Pure` funktioner. Det finns dock bibliotek som `Debug.SimpleReflect` som erbjuder liknande funktioner för pure funktioner.

## Se även

* [Debugging i Haskell](https://www.schoolofhaskell.com/school/starting-with-haskell/debugging-haskell)
* [Debug.Trace dokumentation](https://hackage.haskell.org/package/base-4.14.1.0/docs/Debug-Trace.html)
* [Using Debug.Trace tutorial](https://wiki.haskell.org/Using_Debug.Trace)