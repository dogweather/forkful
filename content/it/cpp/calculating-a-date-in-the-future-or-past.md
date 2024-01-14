---
title:                "C++: Calcolare una data nel futuro o nel passato."
simple_title:         "Calcolare una data nel futuro o nel passato."
programming_language: "C++"
category:             "C++"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/cpp/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

# Perché
Il calcolo di una data nel futuro o nel passato può essere utile in molti casi, ad esempio quando si vuole pianificare un viaggio o un evento, o semplicemente per avere una maggiore comprensione del tempo che scorre.

## Come fare
Per calcolare una data nel futuro o nel passato, è necessario utilizzare delle funzioni specifiche del linguaggio di programmazione. Nel caso di C++, possiamo utilizzare le funzioni della libreria <ctime> come "mktime" e "gmtime" per manipolare le date e ottenere il risultato desiderato.

Un esempio di codice potrebbe essere il seguente:

```C++

#include <iostream>
#include <ctime>
using namespace std;

int main() {
    struct tm date; // Creiamo una struttura che rappresenti una data
    date.tm_year = 2021 - 1900; // Impostiamo l'anno corrente (attenzione: la funzione mktime richiede l'anno - 1900)
    date.tm_mon = 9; // Impostiamo il mese (in questo caso, ottobre)
    date.tm_mday = 16; // Impostiamo il giorno del mese
    date.tm_hour = 12; // Impostiamo l'ora (in questo caso, mezzogiorno)
    date.tm_min = 0; // Impostiamo i minuti
    date.tm_sec = 0; // Impostiamo i secondi

    // Calcoliamo la data nel futuro aggiungendo 10 giorni alla data impostata
    mktime(&date);
    date.tm_mday += 10;

    // Stampiamo il risultato
    cout << "La data nel futuro è: " << asctime(&date) << endl;

    return 0;
}

```

Questo codice stamperà la seguente output:

```
La data nel futuro è: Mon Oct 26 12:00:00 2021
```

## Approfondimento
Per calcolare una data nel futuro o nel passato è importante comprendere il funzionamento delle funzioni utilizzate. Ad esempio, la funzione "mktime" accetta come parametro una struttura "tm" contenente la data che vogliamo manipolare. Questa funzione restituisce un valore di tipo "time_t" che rappresenta il numero di secondi trascorsi dal 1 gennaio 1970. Una volta ottenuto questo valore, possiamo utilizzare la funzione "gmtime" per convertirlo in una struttura "tm" che rappresenti la data desiderata.

È importante notare che quando si utilizzano le funzioni della libreria <ctime>, il conteggio dei mesi parte da 0 (quindi gennaio è rappresentato dal numero 0, febbraio dal numero 1 e così via). Inoltre, poiché le date sono rappresentate in modo diverso in ogni paese, potrebbero essere necessarie modifiche nel codice per ottenere il risultato desiderato.

# Vedi anche
- https://www.cplusplus.com/reference/ctime/mktime/
- https://www.cplusplus.com/reference/ctime/gmtime/