---
title:    "C: Generering av slumpmässiga tal"
keywords: ["C"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/sv/c/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Varför

Att generera slumpmässiga tal är en viktig del av programmering eftersom det gör det möjligt att skapa mångsidiga och dynamiska program. Det hjälper också till att testa programmen och skapa realistiska simuleringar.

## Så här gör du

För att generera slumpmässiga tal i C, använder vi funktionen `rand()` som finns i standardbiblioteket `stdlib.h`. Denna funktion returnerar ett slumpmässigt tal mellan 0 och `RAND_MAX` (ofta mellan 0 och 32767). För att generera ett tal inom ett specifikt intervall använder vi enkel aritmetik, som exemplifieras nedan:

```C
// inkludera standardbiblioteket för att använda rand()
#include <stdlib.h>
// inkludera standardbiblioteket för att använda printf()
#include <stdio.h>

int main()
{
    // generera ett slumpmässigt tal mellan 1 och 10
    int random = rand() % 10 + 1;

    // skriv ut det genererade talet
    printf("Det slumpmässiga talet är: %d\n", random);
    return 0;
}
```

Exempelutgång:

```
Det slumpmässiga talet är: 4
```

## Djupdykning

Det finns flera faktorer som påverkar hur slumpmässiga de genererade talen är. En av dem är startvärdet för `rand()`, som bestäms av `srand()`-funktionen. Om vi inte specificerar ett startvärde kommer `rand()` att använda samma startvärde varje gång programmet körs, vilket betyder att de genererade talen kommer att vara samma varje gång. Det är därför viktigt att använda en variabel som baseras på tiden eller på en annan variabel som kan variera för att få mer slumpmässiga tal.

En annan faktor är att `rand()` inte genererar helt slumpmässiga tal utan är beroende av ett matematiskt samband baserat på startvärdet. Detta kan resultera i att det genererade mönstret kan upprepas efter en viss period. Detta är därför `rand()`-funktionen inte rekommenderas för kryptografiska ändamål.

## Se även

- [rand() dokumentation](https://www.cplusplus.com/reference/cstdlib/rand/)
- [srand() dokumentation](https://www.cplusplus.com/reference/cstdlib/srand/)
- [Random number generation in C](https://www.geeksforgeeks.org/rand-and-srand-in-ccpp/)
- [Using rand() to generate random numbers in C](https://www.techiedelight.com/random-number-generator-c/)