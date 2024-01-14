---
title:                "C: Generering av slumpmässiga tal"
programming_language: "C"
category:             "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/c/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Varför

Att kunna generera slumpmässiga nummer i ditt C-program kan vara en användbar funktion för olika ändamål. Det kan användas för att skapa olika testfall för dina algoritmer, simulera slumpmässiga händelser eller till och med skapa slumpmässiga spel. I denna bloggpost kommer vi att djupdyka i hur du kan generera slumpmässiga nummer i ditt C-program.

## Hur man gör

För att generera slumpmässiga nummer använder vi oss av funktionen `rand()` i C-programmeringsspråket. Detta kommer att returnera ett slumpmässigt nummer i intervallet [0, RAND_MAX]. För att få ett mer varierat resultat kan vi använda oss av funktionen `srand()` som tar en startseed som argument.

Låt oss se ett exempel på kod för att generera ett slumpmässigt heltal mellan 1 och 10:

```C
#include <stdio.h>
#include <stdlib.h>
#include <time.h>

int main()
{
    // Sätter en startseed baserad på tiden
    srand(time(0));

    // Genererar ett slumpmässigt heltal mellan 1 och 10
    int randomNum = 1 + rand() % 10;

    // Skriver ut resultatet
    printf("Slumpmässigt heltal: %d \n", randomNum);

    return 0;
}
```

Detta kommer att ge ett resultat som kan se ut så här:

```
Slumpmässigt heltal: 8
```

## Djupdykning

Det är viktigt att notera att funktionen `rand()` faktiskt inte genererar slumpmässiga nummer, utan en sekvens av nummer som kan förutsägas. Det är därför som vi använder `srand()` för att få ett mer slumpmässigt beteende. Genom att sätta en startseed, baserad på tiden, kommer vi att få en annan sekvens av nummer varje gång programmet körs.

Det finns också andra sätt att generera slumpmässiga nummer med hjälp av t.ex. `rand()` tillsammans med `srand()`. Dessutom är det möjligt att få slumpmässiga flytande eller binära nummer genom att använda lämplig division och konvertering av resultatet från `rand()`. Dessa är mer avancerade tekniker som vi inte kommer att gå in på i denna bloggpost, men som kan vara användbara att utforska om du är intresserad av mer detaljerad information.

## Se även

- [C - Random Number Generation](https://www.tutorialspoint.com/c_standard_library/c_function_rand.htm)
- [Generating random numbers](https://www.geeksforgeeks.org/generating-random-number-range-c/) på GeeksforGeeks
- [Using rand() and srand()](https://www.cplusplus.com/reference/cstdlib/rand/) på Cplusplus.com