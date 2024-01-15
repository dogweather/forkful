---
title:                "Ottenere la data corrente"
html_title:           "C: Ottenere la data corrente"
simple_title:         "Ottenere la data corrente"
programming_language: "C"
category:             "C"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/c/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Perché

Spesso è necessario ottenere la data corrente all'interno di un programma per poter registrare eventi o gestire processi. Inoltre, la conoscenza della data corrente può essere utile per visualizzarla all'utente o per effettuare calcoli basati sulla data.

## Come fare

Per ottenere la data corrente in un programma scritto in linguaggio C, esistono varie opzioni. Una delle più semplici è l'utilizzo della funzione ```time()``` che restituisce il numero totale di secondi trascorsi dal 1 gennaio 1970. Per convertire questo valore in una data leggibile, possiamo utilizzare la funzione ```localtime()``` che accetta come parametro il valore restituito dalla funzione ```time()```. Di seguito un esempio di codice che stampa la data corrente:

```
#include <stdio.h>
#include <time.h>

int main() {

    // Ottieni il numero di secondi trascorsi da 01/01/1970
    time_t current_time = time(NULL);
    
    // Converti in una struttura di tipo tm contenente la data corrente
    struct tm *local_time = localtime(&current_time);
    
    // Stampa la data nel formato desiderato
    printf("La data corrente è: %d/%d/%d", local_time->tm_mday, local_time->tm_mon + 1, local_time->tm_year + 1900);

    return 0;
}
```

Output:

```
La data corrente è: 15/07/2021
```

## Approfondimento

La funzione ```time()``` utilizza un valore di precisione di 1 secondo, quindi se abbiamo bisogno di una maggiore precisione possiamo utilizzare la funzione ```gettimeofday()``` che restituisce il tempo di sistema con un precisione fino al microsecondo. Inoltre, esistono anche altre funzioni utili per effettuare calcoli o manipolazioni sulla data e l'ora, come ad esempio ```mktime()``` che converte una struttura di tipo tm in un valore di tipo time_t.

## Vedi anche

- [Documentazione ufficiale di C](https://devdocs.io/c/)
- [Tutorial su date e ore in C](https://www.tutorialspoint.com/c_standard_library/time_h.htm)
- [Esempi di codice per la gestione delle date in C](https://www.geeksforgeeks.org/date-time-functions-in-c-c/)