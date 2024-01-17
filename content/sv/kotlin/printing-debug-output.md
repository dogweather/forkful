---
title:                "Utskrift av felsökningsutdata"
html_title:           "Kotlin: Utskrift av felsökningsutdata"
simple_title:         "Utskrift av felsökningsutdata"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/kotlin/printing-debug-output.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att skriva ut felsökningsutdata är en process där man skriver ut extra information i koden för att hjälpa till med felsökning av programvaran. Detta görs för att underlätta för utvecklare att hitta och åtgärda fel i koden.

## Hur man gör:
Kotlin har en inbyggd funktion, ```println()```, som används för att skriva ut felsökningsutdata. Den tar ett argument, som kan vara en sträng eller en variabel, och skriver ut värdet på det argumentet i konsolen. 

Exempel:

```Kotlin
var name = "Kotlin"
println("$name är ett programmeringsspråk.")
```
Output:

Kotlin är ett programmeringsspråk.

## Djupdykning:
Att skriva ut debug output är ett vanligt felsökningsverktyg som har funnits sedan programmeringens tidiga dagar. Innan moderna felsökningverktyg fanns tillgängliga var det den enda metoden för att hitta och åtgärda fel i koden. 

Idag finns det också andra alternativ för felsökning, såsom debuggingverktyg, som låter utvecklare steg för steg gå igenom koden för att hitta fel. Men att skriva ut debug output kan fortfarande vara ett användbart verktyg för att snabbt identifiera enkla fel och göra snabba korrigeringar i koden.

Det är viktigt att komma ihåg att debug output inte bör användas i produktionskod, eftersom det kan påverka prestanda och säkerhet. Det är enbart avsett som ett verktyg för felsökning och bör tas bort eller kommenteras ut när koden publiceras.

## Se även:
Officiell Kotlin dokumentation om ```println()```: https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.io/println.html

En guide om felsökning i Kotlin: https://medium.com/@baphemot/whats-the-debugging-like-for-kotlin-be5749f8f817