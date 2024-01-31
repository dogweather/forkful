---
title:                "Arrotondamento dei numeri"
date:                  2024-01-26T03:42:51.514791-07:00
model:                 gpt-4-0125-preview
simple_title:         "Arrotondamento dei numeri"

tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/c/rounding-numbers.md"
---

{{< edit_this_page >}}

## Cosa & Perché?
Arrotondare i numeri significa troncare le cifre oltre un certo punto, opzionalmente aggiustando l'ultima cifra mantenuta. I programmatori arrotondano per ridurre la precisione quando i valori esatti non sono necessari, gestire gli errori dei numeri in virgola mobile o preparare i numeri per una visualizzazione amichevole per l'utente.

## Come fare:
In C, si utilizzano tipicamente le funzioni `floor()`, `ceil()` o `round()`. Ecco una rapida dimostrazione:

```C
#include <stdio.h>
#include <math.h>

int main() {
    double num = 3.14159;
    double num_floor = floor(num);
    double num_ceil = ceil(num);
    double num_round = round(num);

    printf("Floor: %.2f\n", num_floor); // Floor: 3.00
    printf("Ceil: %.2f\n", num_ceil);   // Ceil: 4.00
    printf("Round: %.2f\n", num_round); // Round: 3.00
    return 0;
}
```

Per un controllo maggiore, come arrotondare in un punto specifico, si moltiplica, si arrotonda e si divide:

```C
double roundToPlace(double num, int place) {
    double scale = pow(10.0, place);
    return round(num * scale) / scale;
}

// ...

double num = 3.14159;
double num_arrotondato = roundToPlace(num, 2);
printf("Arrotondato a 2 decimali: %.2f\n", num_arrotondato); // Arrotondato a 2 decimali: 3.14
```

## Approfondimento
Nel passato, arrotondare spesso significava un processo manuale—un compito pesante solo con penna e carta. Con l'informatica, abbiamo automatizzato questo processo, ma l'aritmetica in virgola mobile ha introdotto sfumature a causa della sua natura binaria, dove alcuni numeri non possono essere rappresentati esattamente.

Le alternative all'arrotondamento standard includono la troncatura (eliminazione semplice delle cifre extra) o l'arrotondamento bancario, che arrotonda al numero pari più vicino quando si trova esattamente tra due valori, riducendo il bias nei calcoli ripetuti.

L'implementazione diventa complicata quando è necessario arrotondare numeri con precisione arbitraria o gestire casi speciali come l'infinito, i NaN segnalati, o i valori subnormali. Le funzioni della libreria standard C gestiscono le basi, ma se è necessario arrotondare i decimali in modi personalizzati, avrai bisogno di più di `math.h`.

## Vedi Anche
- Documentazione di [`<math.h>`](https://en.cppreference.com/w/c/numeric/math)
- [Le insidie della verifica dei calcoli in virgola mobile](https://dl.acm.org/doi/10.1145/1186736.1186737)
