---
title:                "Utskrift av felsökningsresultat"
html_title:           "Gleam: Utskrift av felsökningsresultat"
simple_title:         "Utskrift av felsökningsresultat"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/gleam/printing-debug-output.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att skriva ut felsökningsutdata är ett sätt för programmerare att se vad som händer i deras kod under körning. Det är ett användbart verktyg för att hitta och lösa problem.

## Hur gör man:
```Gleam
io_debug.print("Hello world")
```

Detta kodexempel ska skriva ut texten "Hello world" i terminalen när programmet körs.

## Djupdykning:
Att skriva ut felsökningsutdata har funnits länge, i början gjorde programmerare detta genom att använda kommandon som "print" eller "write". Med tiden har det utvecklats till mer sofistikerade metoder som tillåter programmerare att välja vilken typ av data de vill skriva ut.

En alternativ metod att skriva ut felsökningsutdata är att använda en debugger, en programvara som hjälper till att hitta och lösa fel i koden.

I Gleam används funktionen "io_debug.print" för att skriva ut data i terminalen. Det finns också andra funktioner som tillåter mer komplex utdata, som "io_debug.inspect" som visar innehållet i variabler och "io_debug.backtrace" som visar spåret av vilka funktioner som har kallats.

## Se även:
- [Debugging in Gleam](https://gleam.run/book/tour/debugging.html)
- [Introduction to Debugging](https://www.educative.io/courses/introduction-to-debugging)