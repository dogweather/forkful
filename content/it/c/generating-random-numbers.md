---
title:                "Generazione di numeri casuali"
html_title:           "Arduino: Generazione di numeri casuali"
simple_title:         "Generazione di numeri casuali"
programming_language: "C"
category:             "C"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/c/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Cosa & Perché?

La generazione di numeri casuali in programmazione si riferisce alla produzione di numeri che non mostrano alcun pattern prevedibile. Questa pratica è essenziale quando si creano giochi, simulazioni o software di crittografia, per citarne solo alcuni.

## Come fare:

In C, possiamo utilizzare la funzione `rand()` per generare numeri casuali. Ecco un esempio:

```C
#include <stdio.h>
#include <stdlib.h>
#include <time.h>

int main(){
    srand(time(0));  // Inizializziamo il generatore di numeri casuali
    int random_number = rand();  // Generiamo un numero casuale
    printf("Numero casuale: %d\n", random_number);
    return 0;
}
```

Quando esegui questo codice, otterrai un numero casuale diverso ogni volta.

## Approfondimento

La generazione di numeri casuali è un concetto piuttosto vecchio e i metodi per farlo sono molti. Il metodo che abbiamo esaminato sopra utilizza il tempo del sistema per iniziare il generatore di numeri casuali (seme). Tuttavia, è importante notare che i numeri generati attraverso questo metodo non sono veramente casuali, ma pseudo-casuali. Ciò significa che dati lo stesso seme, la sequenza di numeri generata sarà sempre la stessa.

In alternativa, potresti utilizzare numeri primi o algoritmi più complessi per generare numeri casuali, ma `rand()` dovrebbe essere sufficiente per la maggior parte degli usi.

## Links utili

Per ulteriori dettagli e utilizzo avanzato della generazione di numeri casuali in C:

1. [How to Generate Random Numbers in C](https://www.cprogramming.com/tutorial/random.html) 
2. [Random Number Generation in C](https://www.geeksforgeeks.org/rand-and-srand-in-ccpp)
3. [rand - C Reference](https://en.cppreference.com/w/c/numeric/random/rand)