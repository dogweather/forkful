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

## Perché

Ma perché dovresti generare numeri casuali in un programma scritto in C? Beh, ci sono molti motivi per farlo! Ad esempio, potresti aver bisogno di simulare un gioco d'azzardo o di testare l'efficacia di un algoritmo.

## Come

Per generare numeri casuali in C, puoi utilizzare la funzione `rand()` della libreria standard `stdlib.h`. Ecco un esempio di codice che genera 10 numeri casuali compresi tra 1 e 100 e li stampa a schermo:

```C
#include <stdlib.h>
#include <stdio.h>

int main() {
   int i;
   for (i = 0; i < 10; i++) {
      int num = (rand() % 100) + 1;
      printf("%d ", num);
   }
   return 0;
}
```

Ecco un esempio di output:

```
37 56 12 89 24 76 63 45 90 2
```

Questa funzione utilizza un algoritmo per generare numeri pseudo-casuali, quindi se vuoi dei numeri veramente casuali, potresti aver bisogno di utilizzare un generatore di numeri casuali esterno.

## Approfondimento

Come accennato prima, `rand()` utilizza un algoritmo per generare numeri pseudo-casuali. Ciò significa che i numeri non sono veramente casuali, ma sono piuttosto una sequenza deterministica di numeri che sembrano casuali. Ad esempio, se esegui lo stesso codice più volte, otterrai la stessa sequenza di numeri.

Inoltre, `rand()` utilizza un seed (seme) per iniziare la sequenza di numeri. Se non viene fornito un seed, la funzione utilizza di default il valore 1. Ciò significa che se esegui il codice senza specificare un seed, otterrai sempre la stessa sequenza di numeri. Per ottenere una sequenza diversa ogni volta, devi fornire un seed diverso, ad esempio utilizzando il valore corrente dell'orologio come seed.

Puoi anche utilizzare la funzione `srand()` per inizializzare il seed in modo esplicito. Ad esempio, se vuoi generare i numeri casuali in base all'ora corrente, puoi utilizzare il seguente codice:

```C
#include <stdlib.h>
#include <stdio.h>
#include <time.h>

int main() {
   srand(time(0)); // inizializzo il seed basato sull'ora corrente
   int i;
   for (i = 0; i < 10; i++) {
      int num = (rand() % 100) + 1;
      printf("%d ", num);
   }
   return 0;
}
```

## Vedi anche

- La documentazione della funzione `rand()`: https://www.tutorialspoint.com/c_standard_library/c_function_rand.htm
- Informazioni sulla generazione di numeri casuali in C: https://www.geeksforgeeks.org/generating-random-number-range-c/