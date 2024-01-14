---
title:                "C: Generazione di numeri casuali"
programming_language: "C"
category:             "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/c/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Perché

Generare numeri casuali è una pratica comune nella programmazione, poiché spesso è necessario avere a disposizione dei valori casuali per simulare situazioni del mondo reale o creare giochi. Inoltre, l'utilizzo di numeri casuali può aiutare a rendere i programmi più imprevedibili e interessanti per l'utente.

## Come fare

Per generare numeri casuali in C, è necessario utilizzare la funzione `rand()` della libreria `stdlib.h`. Questa funzione restituisce un numero intero casuale compreso tra 0 e `RAND_MAX`, il quale può variare in base al sistema operativo utilizzato.

Di seguito è riportato un esempio di codice che genera 5 numeri casuali e li stampa a schermo:

```C
#include <stdio.h>
#include <stdlib.h>

int main()
{
    int i;
    // generazione dei numeri casuali
    for (i = 0; i < 5; i++) {
        int num = rand();
        printf("Numero casuale %d: %d\n", i+1, num);
    }
    return 0;
}
```

Output:

```
Numero casuale 1: 25821
Numero casuale 2: 18040
Numero casuale 3: 7434
Numero casuale 4: 34031
Numero casuale 5: 10139
```

Si noti che i numeri generati sono diversi ad ogni esecuzione del programma, poiché vengono scelti casualmente.

## Approfondimento

La generazione di numeri casuali avviene tramite l'utilizzo di un algoritmo matematico che produce una sequenza di numeri pseudo-casuali. Questo significa che i numeri non sono veramente casuali, ma la sequenza può sembrare casuale per l'utente. Inoltre, l'algoritmo utilizzato da `rand()` è influenzato da una variabile di stato interna chiamata "seed", che di solito viene inizializzata con un valore diverso ad ogni avvio del programma.

Per questo motivo, è importante utilizzare la funzione `srand()` per impostare manualmente il valore del seed e garantire una maggiore casualità nella sequenza dei numeri generati. È possibile passare come argomento il valore di `time(NULL)`, che restituisce il numero di secondi trascorsi dal 1 gennaio 1970, per ottenere un seed differente ad ogni esecuzione del programma.

Un altro modo per migliorare l'imprevedibilità dei numeri casuali è quello di utilizzare una funzione di hash come `rand_r()`, che richiede un parametro aggiuntivo da utilizzare come seed.

Inoltre, per ottenere numeri casuali in un range specifico, si può utilizzare il resto della divisione del numero generato per il valore massimo desiderato. Ad esempio, per ottenere numeri tra 0 e 9, si può utilizzare `rand() % 10`.

## Vedi anche

- Documentazione ufficiale sulla funzione `rand()`: https://www.cplusplus.com/reference/cstdlib/rand/
- Approfondimenti sulla generazione di numeri casuali in C: https://www.geeksforgeeks.org/understanding-rand-function-in-c-and-its-implementation/