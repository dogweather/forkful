---
title:                "Skriva ut felsökningsresultat"
html_title:           "Fish Shell: Skriva ut felsökningsresultat"
simple_title:         "Skriva ut felsökningsresultat"
programming_language: "C"
category:             "C"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/c/printing-debug-output.md"
---

{{< edit_this_page >}}

# Att Skriva Ut Debugoutput i C

## Vad & Varför?
Att skriva ut debug output är ett sätt att spåra och lösa problem i koden genom att visa variabelvärden och programflödet. Det hjälper programmeraren att förstå vad programmet gör och var det möjligtvis går fel.

## Hur man gör:
Här är ett grundläggande exempel på hur man skriver ut debug output med hjälp av `printf` funktionen i C.

```C
#include <stdio.h>

int main() {
    int i = 5;
    printf("Debug: i is %d\n", i);  // Skriver ut "Debug: i is 5"
    return 0;
}
```

Lägg märke till att vi använder `printf` för att skriva ut variabelvärdet. På detta sätt kan vi följa vad som händer med variabeln `i` i programmet.

## Fördjupning
Vi lägger nu märke till någrar punkter. För det första, användandet av debug utskrifter är en gammal teknik, men ändå kraftfull och användbar. Faktum är att det började användas i början av programmeringshistorien när mer sofistikerade debuggers inte fanns.

Det finns alternativ till att använda `printf` för debug utskrift. Ett av dessa alternativ är användning av debuggers, som GDB, vilka erbjuder mycket mer kraftfulla verktyg för felsökning men kan vara mer komplicerade att använda.

När det gäller implementation, se till att du tar bort debug utskrifter från din kod när du är klar. Ett sätt att hantera detta på är att ha en `DEBUG` flagga som du kan ställa in för att aktivera eller inaktivera debug utskrifter.

## Se också
För mer information om C programmering och debug tekniker, se följande källor:

- [GDB: The GNU Project Debugger](https://www.gnu.org/software/gdb/) 