---
title:                "Generera slumpmässiga tal"
date:                  2024-01-20T17:48:58.241714-07:00
model:                 gpt-4-1106-preview
simple_title:         "Generera slumpmässiga tal"
programming_language: "C++"
category:             "C++"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/cpp/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Vad & Varför?

Att generera slumpmässiga tal är en process att skapa tal som inte kan förutsägas logiskt - de är, som namnet antyder, slumpade. Programmerare använder detta för att simulera händelser, testa algoritmer, och skapa mer dynamiska och oförutsägbara upplevelser i spel och applikationer.

## Hur man gör:

Generering av slumpmässiga tal i C++ kan göras med `<random>` biblioteket. Exemplet nedan visar hur man genererar ett enkelt slumptal i intervallet [0, 99].

```C++
#include <iostream>
#include <random>

int main() {
    std::random_device rd;  // Startpunkt för att skapa slumptal
    std::mt19937 gen(rd()); // Mersenne Twister-generator
    std::uniform_int_distribution<> dis(0, 99); // Jämn fördelning över intervall

    int slumptal = dis(gen); // Generera slumptal
    std::cout << "Slumptal: " << slumptal << std::endl;

    return 0;
}
```

Exempel på utdata: `Slumptal: 42`

## Fördjupning:

Historiskt har C++ använt funktionen `rand()` från C-standarden men det är känd för sina brister i slumpmässigheten och dålig distribution. Med C++11 introducerades `<random>` biblioteket som ger mer tillförlitliga och flexibla sätt att generera slumpmässiga tal. Biblioteket inkluderar en mängd generatorer och distributioner, såsom Mersenne Twister (en snabb pseudo-slumptalsgenerator) och olika fördelningar som till exempel normalfördelning eller binomialfördelning.

Implementationsdetaljer varierar beroende på vilken generator och distribution som används. Det är även viktigt att initialisera en generator (så som `std::mt19937` med ett slumpmässigt startvärde, ofta hämtat från `std::random_device`) för att undvika samma sekvens av slumptal varje körning.

För andra användningsfall, exempelvis kryptografi, är det bättre att använda specialiserade bibliotek som tillhandahåller krypto-säkra slumptalsgeneratorer.

## Se också:

- C++ Standardbiblioteket för `<random>` för detaljer om olika generatorer och distributioner: https://en.cppreference.com/w/cpp/header/random
- Diskussioner om slumptalsgenerering i C++ på Stack Overflow: https://stackoverflow.com/questions/tagged/random+c%2b%2b
- En artikel om varför `rand()` anses vara dålig och hur man kan använda `<random>` istället: https://www.learncpp.com/cpp-tutorial/random-number-generation/
