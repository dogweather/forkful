---
title:                "C: Confrontare due date"
simple_title:         "Confrontare due date"
programming_language: "C"
category:             "C"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/c/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Perché
Una delle operazioni più comuni nella programmazione è confrontare due date. Ciò può essere utile per vari scopi, come ad esempio il calcolo di un'età, l'ordinamento di eventi cronologici o la gestione di scadenze. In questo post esploreremo come confrontare due date utilizzando il linguaggio di programmazione C.

## Come Fare
Per confrontare due date in C, è necessario utilizzare la funzione `difftime()` della libreria `time.h`. Questa funzione prende come argomenti due variabili di tipo `time_t` che rappresentano le due date da confrontare e restituisce il numero di secondi trascorsi tra queste due date.

Ecco un esempio di come utilizzare la funzione `difftime()`:

```
#include <stdio.h>
#include <time.h>

int main()
{
    // Definiamo due variabili di tipo time_t
    time_t data1, data2;
    
    // Assegna una data alla prima variabile
    data1 = time(0);
    
    // Assegna una data successiva alla seconda variabile
    data2 = data1 + (24 * 60 * 60); // Aggiunge 24 ore
    
    // Confronta le due date utilizzando la funzione difftime()
    double differenza = difftime(data2, data1);
    
    // Output: Differenza in secondi tra le due date
    printf("La differenza tra le due date è di %f secondi.\n", differenza);
    
    return 0;
}
```

Output:

```
La differenza tra le due date è di 86400.000000 secondi.
```

## Approfondimento
La funzione `difftime()` è in grado di confrontare qualsiasi tipo di dato `time_t`, come ad esempio date in formato Unix o date rappresentate da una stringa. Inoltre, è possibile ottenere la differenza tra le due date in diversi formati, come ad esempio in giorni, ore o minuti.

Per ulteriori informazioni sulla gestione delle date in C, si consiglia di consultare la documentazione ufficiale della libreria `time.h`.

## Vedi Anche
- [Tutorial: Gestione delle date in C](https://www.programiz.com/c-programming/c-date-time)
- [Documentazione ufficiale: funzioni della libreria time.h](https://www.tutorialspoint.com/c_standard_library/time_h.htm)
- [Come confrontare due date in altri linguaggi di programmazione](https://www.techwalla.com/articles/how-to-compare-two-dates-in-different-programming-languages)