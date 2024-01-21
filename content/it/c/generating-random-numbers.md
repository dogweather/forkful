---
title:                "Generazione di numeri casuali"
date:                  2024-01-20T17:48:33.795183-07:00
model:                 gpt-4-1106-preview
simple_title:         "Generazione di numeri casuali"
programming_language: "C"
category:             "C"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/c/generating-random-numbers.md"
---

{{< edit_this_page >}}

## What & Why?
Generare numeri casuali significa creare valori che non possono essere previsti logicamente. Il perché? Utile per simulazioni, giochi e sicurezza dei dati, ovviamente.

## How to:
Ecco un esempio semplice in C:

```C
#include <stdio.h>
#include <stdlib.h>
#include <time.h>

int main() {
    // Inizializza il generatore di numeri casuali
    time_t t;
    srand((unsigned) time(&t));

    // Genera e stampa un numero casuale tra 0 e 99
    printf("Numero casuale: %d\n", rand() % 100);

    return 0;
}
```

Esempio di output:
```
Numero casuale: 42
```

## Deep Dive
Dal 1972, la funzione `rand()` viene utilizzata in C, ma ha i suoi limiti, come una distribuzione non uniforme. Alternative moderne includono `/dev/random` su Unix o `RtlGenRandom()` su Windows. Implementazione corretta richiede una buona fonte di entropia e algoritmi sofisticati come Mersenne Twister o generatori basati su crittografia.

## See Also
- [cplusplus.com - rand](http://www.cplusplus.com/reference/cstdlib/rand/)
- [Wikipedia - Random number generation](https://en.wikipedia.org/wiki/Random_number_generation)
- [Stack Overflow - How to generate a random number in C?](https://stackoverflow.com/questions/822323/how-to-generate-a-random-number-in-c)