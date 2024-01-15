---
title:                "Cancellazione di caratteri corrispondenti a un modello."
html_title:           "C: Cancellazione di caratteri corrispondenti a un modello."
simple_title:         "Cancellazione di caratteri corrispondenti a un modello."
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/c/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Perché

Se sei un programmatore in C, potresti a volte aver bisogno di eliminare caratteri da una stringa che corrispondono a un certo modello. Questo potrebbe essere necessario per filtrare dati o per riformattare una stringa.

## Come fare

Per eliminare caratteri che corrispondono a un modello in una stringa, puoi utilizzare la funzione `strsep()` della libreria standard di C. Qui di seguito un esempio di codice che mostra come utilizzarla:

```C
#include <stdio.h>
#include <string.h>

int main(void) {
    // Definire una stringa di esempio
    char stringa[] = "Questa è una stringa di esempio.";

    // Definire il modello da eliminare
    char pattern[] = "aeiou";

    // Ciclo per eliminare ogni carattere del modello dalla stringa
    char* token;
    while ((token = strsep(&stringa, pattern)) != NULL) {
        printf("%s", token);
    }
    printf("\n");

    return 0;
}
```

Questo codice restituirà la stringa "Qst  strng d mpl.". Puoi modificare il modello e la stringa di esempio per ottenere risultati diversi.

## Approfondimento

La funzione `strsep()` è una funzione molto utile quando si lavora con stringhe in C. In realtà, è una versione migliorata della funzione `strtok()` che, invece di modificare la stringa originale, restituisce ogni token estratto in una nuova stringa. Questo permette di lavorare su una stringa senza modificarla.

Vale la pena notare che, se non ci sono più corrispondenze tra il modello e la stringa, la funzione restituisce `NULL`. È importante gestire questo caso all'interno del tuo codice per evitare errori.

## Vedi anche

- [Documentazione ufficiale della funzione `strsep()`](https://en.cppreference.com/w/c/string/byte/strsep)
- [Esempi di utilizzo della funzione `strsep()`](https://www.geeksforgeeks.org/strsep-in-c-strtok/)
- [Come utilizzare le stringhe in C](https://www.programiz.com/c-programming/c-strings)