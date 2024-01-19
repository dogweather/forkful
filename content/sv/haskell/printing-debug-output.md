---
title:                "Skriva ut felsökningsresultat"
html_title:           "Fish Shell: Skriva ut felsökningsresultat"
simple_title:         "Skriva ut felsökningsresultat"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/haskell/printing-debug-output.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Skriva ut debug-utdata är handlingen att skicka information om ditt program till en utdataström, såsom en konsol, för att spåra programmet under drift. Detta gör det möjligt för programmerare att effektivt hitta och fixa eventuella fel i koden.

## Hur man gör det:
Här är ett enkelt exempel som visar hur du kan skriva ut debug-utdata i Haskell:

```Haskell
import Debug.Trace

main :: IO ()
main = print (debug "Hej världen!")

debug :: String -> String
debug msg = trace msg msg
```

Kör denna kod och den kommer att skriva ut "Hej världen!" på din konsol. `trace` funktionen tar två argument, första argumentet är meddelandet du vill skriva ut och det andra argumentet är värdet du vill returnera från funktionen. Här returnerar den samma meddelande som den skriver ut.

## Djupdykning

`trace` funktionen i Haskell kommer från `Debug.Trace` modulen. Historiskt sett har den använts som en snabb-och-smutsig lösning för att skriva ut debug-meddelanden. Samtidigt är det dock inte den enda lösningen—`Debug.Trace` modulen innehåller flera andra funktioner som `traceShow`, `traceShowId`, och `traceStack` för mer detaljerade utskrifter.

En viktig sak att notera är att `trace` och liknande funktioner strider mot Haskells rena natur, eftersom de skapar sidoeffekter. De bör därför användas sparsamt och bara för felsökning. Kom ihåg att ta bort dem innan du skjuter upp koden.

## Se vidare

För ytterligare läsning kan du kolla in dokumentationen för `Debug.Trace` modulen:
https://hackage.haskell.org/package/base-4.14.1.0/docs/Debug-Trace.html

Eller denna bloggpost som diskuterar olika metoder för felsökning i Haskell:
https://wiki.haskell.org/Debugging

Hoppas att detta hjälper dig att förstå better utskrift av debug-utdata i Haskell. Lycka till med felsökningen!