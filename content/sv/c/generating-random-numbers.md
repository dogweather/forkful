---
title:                "Generering av slumpmässiga tal"
html_title:           "C: Generering av slumpmässiga tal"
simple_title:         "Generering av slumpmässiga tal"
programming_language: "C"
category:             "C"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/c/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Generering av slumpmässiga nummer är en teknik som används av programmerare för att skapa en sekvens av nummer som verkar slumpmässiga. Detta kan vara användbart för många olika ändamål, som till exempel spelutveckling eller simuleringar.

## Hur gör man:
Här är ett enkelt exempel på hur man genererar ett slumpmässigt heltal mellan 1 och 100 i C:

```C
#include <stdio.h>
#include <stdlib.h>
#include <time.h>

int main()
{
    // sätt en seed baserat på aktuell tid
    srand(time(NULL));

    // generera ett slumpmässigt heltal mellan 1 och 100
    int random_number = rand() % 100 + 1;

    // skriv ut det slumpmässiga numret
    printf("Slumpmässigt nummer: %d\n", random_number);

    return 0;
}
```
Output: "Slumpmässigt nummer: 72"

För att få olika slumpmässiga nummer vid varje körning av programmet, behöver vi använda funktionen `srand` tillsammans med en variabel som innehåller en aktuell tid. Sedan kan vi använda funktionen `rand` tillsammans med modulus-operatorn `%` för att få ett tal inom ett visst intervall.

## Djupdykning:
Att generera slumpmässiga nummer är en viktig del av datorprogrammering och har använts sedan tidiga dagar av IT. Tidigare användes ofta fysiska slumpmässiga processer, som att kasta tärningar, men nu används algoritmer som kan generera pseudoslumpmässiga nummer. Det finns även andra metoder för att generera slumpmässiga tal, som till exempel användning av sensorer eller mätningar av yttre faktorer som temperatur eller atmosfärstryck.

## Se även:
Om du vill lära dig mer om slumpmässiga nummer och dess användning i C, se följande källor:
- [Wikipedia: Random number generation](https://en.wikipedia.org/wiki/Random_number_generation)
- [GeeksforGeeks: rand() and srand() in C/C++](https://www.geeksforgeeks.org/rand-and-srand-in-ccpp/)
- [C-programmering för nybörjare: Ett steg i taget - Slumpmässiga nummer](https://www.cprogramming.com/tutorial/random.html)