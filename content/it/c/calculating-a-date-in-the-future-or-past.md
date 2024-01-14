---
title:                "C: Calcolare una data nel futuro o nel passato"
programming_language: "C"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/c/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Perché

Ci sono molte ragioni per cui potresti essere interessato a calcolare una data nel futuro o nel passato. Forse stai lavorando su un progetto che richiede il calcolo di scadenze o periodi di tempo specifici. Oppure, potresti essere semplicemente una persona curiosa e vuoi saperne di più sulle funzioni di data del linguaggio di programmazione C. In entrambi i casi, questo post è per te!

## Come fare

Per calcolare una data nel futuro o nel passato in C, è necessario utilizzare alcune funzioni di data predefinite, come "time.h". Vediamo un semplice esempio di come utilizzare queste funzioni per calcolare la data di un determinato numero di giorni nel futuro:

```
#include <stdio.h>
#include <time.h>

int main() {
    // Definiamo il numero di giorni nel futuro
    int giorni = 30;
    // Otteniamo la data attuale
    time_t now = time(NULL);
    // Aggiungiamo il numero di giorni e otteniamo la nuova data
    now += giorni * 24 * 60 * 60;
    // Convertiamo la data in una stringa leggibile
    char* future_date = ctime(&now);
    // Stampiamo la data ottenuta
    printf("La data di oggi nel futuro di %d giorni è: %s", giorni, future_date);

    return 0;
}
```

L'output di questo codice sarà:

```
La data di oggi nel futuro di 30 giorni è: Tue Oct 12 17:42:53 2021
```

Puoi anche calcolare una data nel passato utilizzando lo stesso approccio, ma sottraendo invece il numero di giorni. Sperimenta con diversi valori e vedrai che il codice funziona correttamente.

## Deep dive

Ora che hai una comprensione generale di come calcolare una data nel futuro o nel passato, diamo uno sguardo più da vicino alle funzioni che abbiamo utilizzato e come funzionano.

La funzione "time.h" contiene una serie di funzioni utili per la gestione delle date e dei tempi. La funzione "time()" viene utilizzata per ottenere il numero di secondi trascorsi dal 1 ° gennaio 1970. Questa è chiamata "Epoch time" ed è un concetto importante nella gestione delle date e dei tempi nei sistemi informatici.

La funzione "ctime()" viene utilizzata per convertire il numero di secondi in una stringa leggibile. Questa funzione è molto utile per stampare la data e l'orario sullo schermo in un formato comprensibile.

## Vedere anche

- [Documentazione ufficiale di C su time.h](https://www.tutorialspoint.com/c_standard_library/time_h.htm)
- [Approfondimento sull'Epoch time](https://en.wikipedia.org/wiki/Unix_time)
- [Tutorial su come gestire le date e i tempi in C](https://www.codeproject.com/Articles/5851/Working-with-Dates-and-Times-using-C)