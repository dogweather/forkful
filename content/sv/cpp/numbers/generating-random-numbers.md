---
date: 2024-01-27 20:32:53.995293-07:00
description: "Hur man g\xF6r: F\xF6r att generera slumpm\xE4ssiga tal i C++ anv\xE4\
  nder man sig vanligtvis av `<random>`-headern, som introducerades i C++11, och som\
  \ erbjuder ett\u2026"
lastmod: '2024-03-13T22:44:38.205019-06:00'
model: gpt-4-0125-preview
summary: "F\xF6r att generera slumpm\xE4ssiga tal i C++ anv\xE4nder man sig vanligtvis\
  \ av `<random>`-headern, som introducerades i C++11, och som erbjuder ett brett\
  \ utbud av faciliteter f\xF6r att generera slumpm\xE4ssiga tal fr\xE5n olika distributioner."
title: Generera slumptal
weight: 12
---

## Hur man gör:
För att generera slumpmässiga tal i C++ använder man sig vanligtvis av `<random>`-headern, som introducerades i C++11, och som erbjuder ett brett utbud av faciliteter för att generera slumpmässiga tal från olika distributioner.

```C++
#include <iostream>
#include <random>

int main() {
    // Initiera en slumpgenerator
    std::random_device rd;  
    std::mt19937 gen(rd()); 

    // Definiera intervallet [0, 99] inklusive
    std::uniform_int_distribution<> distrib(0, 99); 

    // Generera och skriv ut 5 slumpmässiga tal inom det definierade intervallet
    for(int n=0; n<5; ++n)
        std::cout << distrib(gen) << ' ';
    return 0;
}
```

Denna kodexempel initierar en Mersenne Twister-slumptalsgenerator med ett frö från `std::random_device`. Sedan definieras en likformig heltalsdistribution i intervallet [0, 99] och slutligen skrivs 5 slumpmässiga tal ut från denna distribution.

Exempel på utdata kan se ut så här, men kom ihåg att varje körning sannolikt kommer att producera olika resultat:

```
45 67 32 23 88
```

## Fördjupning:
Historiskt sett har generering av slumpmässiga tal i C++ i hög grad förlitat sig på funktionen `rand()` och funktionen `srand()` för seedning, som finns i `<cstdlib>`-headern. Denna metod har dock ofta kritiserats för sin brist på uniformitet och förutsägbarhet i fördelningen av genererade tal.

Introduktionen av `<random>`-headern i C++11 markerade en betydande förbättring och erbjöd ett sofistikerat system för att producera slumpmässiga tal. De faciliteter som tillhandahålls inkluderar en mängd motorer (som `std::mt19937` för Mersenne Twister) och distributioner (som `std::uniform_int_distribution` för likformig fördelning av heltal) som kan kombineras för att passa programmörens specifika behov, vilket leder till mer förutsägbart beteende, bättre prestanda och större flexibilitet.

Även om `<random>`-biblioteket är mycket bättre än den äldre `rand()`-metoden, är det värt att notera att generering av verkligt slumpmässiga tal – särskilt för kryptografiska ändamål – fortfarande bygger på ytterligare överväganden. För kryptografiska applikationer bör bibliotek som specifikt är utformade för säkerhet och som ofta använder sig av hårdvarubaserade entropikällor användas istället.
