---
title:                "C: Generering av slumpmässiga nummer"
simple_title:         "Generering av slumpmässiga nummer"
programming_language: "C"
category:             "C"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/c/generating-random-numbers.md"
---

{{< edit_this_page >}}

# Varför generera slumpmässiga nummer?

Slumpmässiga nummer är en viktig del av programmering och används för en mängd olika ändamål. Genom att generera slumpmässiga nummer kan du simulera verkliga situationer, skapa unika identifieringsnummer och lösenord, och testa dina program för olika scenarier. Det är även en rolig och spännande utmaning för många programmerare att skriva kod som kan producera slumpmässiga nummer.

## Hur man genererar slumpmässiga nummer

För att generera slumpmässiga nummer i C kan du använda funktionen `rand()`. Denna funktion genererar ett heltal mellan 0 och `RAND_MAX`, vilket vanligtvis är det största möjliga heltal på ditt system. För att använda denna funktion måste du inkludera standardbiblioteket `stdlib.h` och dessutom initiera en startpunkt för din slumpmässiga sekvens genom att använda funktionen `srand()`. Ett vanligt sätt att initiera denna sekvens är med hjälp av det aktuella systemtiden som seed-värde.

```C
#include <stdio.h>
#include <stdlib.h>
#include <time.h>

int main() {
    // Initiera en slumpmässig sekvens baserat på systemtiden
    srand(time(0));

    // Generera 10 slumpmässiga heltal mellan 0 och 99
    for (int i = 0; i < 10; i++) {
        int random_num = rand() % 100; // ger ett heltal mellan 0 och 99
        printf("%d\n", random_num);
    }

    return 0;
}
```

Exempeloutput:

```
53
6
87
41
59
99
76
86
11
38
```

## Grundläggande information om att generera slumpmässiga nummer

För att förstå hur slumpmässiga nummer genereras i C är det viktigt att förstå vad en slumpmässig sekvens är. En slumpmässig sekvens består av en serie av nummer, där varje nummer är beroende av det föregående numret. Dessa nummer är återkommande och förutsägbara, men för att simulera verkliga slumpmässiga händelser är det tillräckligt att denna sekvens är tillräckligt lång och börjar på en annan punkt varje gång programmet körs.

För att generera ett nummer baseras på den föregående i sekvensen, används en matematisk formel som kallas en "pseudo-random number generator" (PRNG). Det finns flera olika algoritmer för PRNG som kan användas i C, men var och en har sina egna egenskaper och framkallar olika typer av slumpmässiga nummer. Det är viktigt att välja rätt algoritm beroende på dina behov.

## Se även

- [Random number generation in C](https://www.geeksforgeeks.org/rand-and-srand-in-ccpp/)
- [Understanding Random Number Generators](https://www.pcg-random.org/)
- [Different types of Random Number Generators](https://codingnest.com/generating-random-non-repeating-integers/)