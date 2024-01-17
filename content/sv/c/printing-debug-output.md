---
title:                "Utskrift av felsökningsutmatning"
html_title:           "C: Utskrift av felsökningsutmatning"
simple_title:         "Utskrift av felsökningsutmatning"
programming_language: "C"
category:             "C"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/c/printing-debug-output.md"
---

{{< edit_this_page >}}

# Debugutskrift i C-programmering

## Vad & Varför?
Debugutskrift är en metod för att lägga till extra kod i ditt C-program för att få ut utskrifter av variabler eller viktiga steg i koden under körning. Detta är ett vanligt sätt för programmerare att felsöka och förstå vad som händer i programmet.

## Så här gör du:
För att lägga till utskrifter i ditt C-program, använd `printf()` funktionen. Du kan skriva ut text eller värden av variabler genom att inkludera dem inuti citattecken eller genom att använda speciella konverterare (%d, %f, osv.) för variabler.

Till exempel, om du vill skriva ut värdet av variabeln `x`, kan du skriva:

```C
printf("x = %d\n", x);
```

Detta kommer att skriva ut värdet av `x` följt av en radbrytning.

## Djupdykning:
Printing debug output är ett vanligt sätt att felsöka i C-programmering eftersom det är enkelt att implementera och ge en ögonblicksbild av variabler och koden. Alternativ till att använda `printf()` inkluderar att använda en debugger eller att logga utskrifter till en fil.

När du lägger till debugutskrifter i din kod, se till att ta bort dem när du är klar med felsökningen, annars kan det påverka prestandan av ditt program.

## Se även:
- [De olika formateringskonverterarna i C](https://www.tutorialspoint.com/c_standard_library/c_function_printf.htm)
- [Introduktion till felsökning i C](https://www.geeksforgeeks.org/cpp-debugging-tips/)
- [C debuggin verktyg](https://www.thegeekstuff.com/2010/03/debug-c-program-using-gdb/)