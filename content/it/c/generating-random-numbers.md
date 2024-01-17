---
title:                "Generazione di numeri casuali"
html_title:           "C: Generazione di numeri casuali"
simple_title:         "Generazione di numeri casuali"
programming_language: "C"
category:             "C"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/c/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Che cos'è e perché: 

Generare numeri casuali è una tecnica usata dai programmatori per creare un'alternativa casuale ai valori predefiniti in un programma. Questo è utile in molte situazioni, come ad esempio per la generazione di password casuali o per testare la casualità di un algoritmo.

## Come fare: 

Per generare un numero casuale in C, è possibile utilizzare la funzione `rand()` della libreria standard. Ad esempio, il codice seguente genera un numero casuale compreso tra 1 e 10:

```C 
#include <stdio.h>
#include <stdlib.h>

int main() {
    int numero_casuale = rand() % 10 + 1;
    printf("Il numero casuale è: %d\n", numero_casuale);
    return 0;
}
```

Output: Il numero casuale è: 7

## Approfondimento: 

La generazione di numeri casuali risale ai primi giorni della computer science ed è stata usata fin dall'inizio per scopi crittografici e di simulazione. Oltre alla funzione `rand()`, esistono anche altre tecniche per la generazione di numeri casuali, come ad esempio l'utilizzo di generatori di numeri pseudo-casuali. Inoltre, è importante notare che i numeri generati con la funzione `rand()` rispettano una distribuzione uniforme e non sono veramente casuali.

## Vedi anche: 

- [Documentazione della funzione `rand()` in C](https://www.tutorialspoint.com/c_standard_library/c_function_rand.htm)
- [Generazione di numeri casuali in C](https://www.geeksforgeeks.org/rand-and-srand-in-ccpp/)
- [Introduzione alla generazione di numeri casuali](https://www.cs.uic.edu/~wilkinson/TheMathematicaJournal/V9I2/random.pdf)