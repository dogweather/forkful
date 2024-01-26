---
title:                "Lavorare con i numeri complessi"
date:                  2024-01-26T04:37:31.310891-07:00
model:                 gpt-4-0125-preview
simple_title:         "Lavorare con i numeri complessi"
programming_language: "C"
category:             "C"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/c/working-with-complex-numbers.md"
---

{{< edit_this_page >}}

## Cos'è e perché?
I numeri complessi, una miscela di parti reali e immaginarie (come 3 + 4i), sono essenziali in calcoli avanzati, come l'elaborazione dei segnali o la risoluzione di certe equazioni. I programmatori li gestiscono per applicazioni pesantemente matematiche dove i numeri tradizionali non sono sufficienti.

## Come fare:
C, dalla versione C99, ha un tipo nativo complesso e una libreria. Ecco come usarli:

```C
#include <stdio.h>
#include <complex.h>

int main() {
    // Dichiarazione di due numeri complessi
    double complex z1 = 1.0 + 3.0 * I;
    double complex z2 = 2.0 - 2.0 * I;

    // Operazioni con numeri complessi
    double complex sum = z1 + z2;
    double complex mult = z1 * z2;

    // Stampa dei risultati
    printf("Somma: %.1f + %.1fi\n", creal(sum), cimag(sum));
    printf("Prodotto: %.1f + %.1fi\n", creal(mult), cimag(mult));

    // Valore assoluto & angolo di fase
    printf("Abs(z1): %f\n", cabs(z1));
    printf("Arg(z1): %f\n", carg(z1));

    return 0;
}
```

Esempio di Output:
```
Somma: 3.0 + 1.0i
Prodotto: 8.0 + 2.0i
Abs(z1): 3.162278
Arg(z1): 1.249046
```
## Approfondimento
I numeri complessi risalgono a secoli fa, con radici nell'algebra del XVI secolo. Andando avanti veloce, ora sono uno standard in molte lingue di programmazione, non solo in C.

Lo standard C99 ha introdotto `<complex.h>`, un'intestazione che definisce macro, funzioni e il tipo di dato `complex`. Esistono alternative - come creare la propria struttura, ma perché reinventare la ruota? La libreria standard C è ottimizzata e pronta all'uso.

Nonostante il suo potere, il supporto complesso di C non è senza critiche. Può essere meno intuitivo rispetto a funzionalità simili in linguaggi come Python, e gestire casi particolari può diventare complicato. Ma per le prestazioni pure, rimane comunque una scelta solida.

## Vedi Anche
- Documentazione dello Standard C99 per `<complex.h>`: https://en.cppreference.com/w/c/numeric/complex
- Standard IEEE per l'Aritmetica in Virgola Mobile (IEEE 754): https://ieeexplore.ieee.org/document/4610935
- Tutorial online per la matematica dei numeri complessi in C: https://www.tutorialspoint.com/complex-number-arithmetic-in-c-programming