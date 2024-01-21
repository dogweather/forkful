---
title:                "Generera slumpmässiga tal"
date:                  2024-01-20T17:48:46.565455-07:00
model:                 gpt-4-1106-preview
simple_title:         "Generera slumpmässiga tal"
programming_language: "C"
category:             "C"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/c/generating-random-numbers.md"
---

{{< edit_this_page >}}

## What & Why? (Vad & Varför?)
Att generera slumpmässiga tal i C innebär att skapa tal som ser oordnade ut. Programmerare använder det för spel, simuleringar och säkerhet där förutsägbarhet är dåligt.

## How to: (Hur man gör:)
För att generera ett slumpmässigt tal i C använder vi `rand()`-funktionen, men kom ihåg att sätta ett frö med `srand()` för äkta slumptal. Så här kan det se ut:

```c
#include <stdio.h>
#include <stdlib.h>
#include <time.h>

int main() {
    // Initialisera slumptalsgenereringen
    srand((unsigned int)time(NULL));

    // Skapa ett slumpmässigt tal mellan 0 och 99
    int randomNummer = rand() % 100;
    
    printf("Slumpmässigt tal: %d\n", randomNummer);
    
    return 0;
}
```

Koden ovan genererar ett nytt slumptal varje gång programmet körs.

## Deep Dive (Djupdykning)
Slumptalsgeneratorer i datorer är inte verkligt slumpmässiga, de är pseudoslumpmässiga. Funktionen `rand()` i C låter som om den producerar slumpmässiga tal men följer en bestämd sekvens (bestämd av startvärdet, eller fröet). Använd `srand()` för att sätta fröet baserat på klockan (`time(NULL)`) vilket ger olika sekvenser vid varje körning. Historiskt har brister i `rand()` lett till sökandet efter bättre alternativ som Mersenne Twister eller cryptografiskt säkra generatorer. I vissa säkerhetskritiska sammanhang används hårdvara för att generera verkligt slumpmässiga tal baserade på fysikaliska fenomen.

## See Also (Se också)
- C Standard Library documentation for `rand()` and `srand()`: https://en.cppreference.com/w/c/numeric/random/rand
- General randomness generation techniques: https://www.random.org/
- Cryptographic random number generators: https://csrc.nist.gov/projects/random-bit-generation