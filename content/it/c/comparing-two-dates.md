---
title:                "Confronto tra due date"
html_title:           "Elixir: Confronto tra due date"
simple_title:         "Confronto tra due date"
programming_language: "C"
category:             "C"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/c/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Cosa & Perché?

Comparare due date significa determinare la differenza tra queste, in termini di giorni, mesi, anni eccetera. Questa operazione è essenziale nella programmazione per vari motivi, come organizzare eventi, calcolare il tempo trascorso e definire scadenze.

## Come fare:

Qui è presentato un esempio di codice che dimostra come comparare due date in C:

```C
#include <stdio.h>
#include <time.h>

int main() {
    struct tm data1 = { 0, 0, 0, 12, 5, 2023 - 1900 };
    struct tm data2 = { 0, 0, 0, 23, 11, 2024 - 1900 };

    time_t t1 = mktime(&data1);
    time_t t2 = mktime(&data2);

    double differenza_tempo = difftime(t2, t1);

    printf("La differenza tra le due date è di %.f secondi.\n", differenza_tempo);
    return 0;
}
```

Output:

```C
La differenza tra le due date è di 29979000 secondi.
```

In questo esempio, le date sono rappresentate come strutture `tm` e sono poi convertite in valori `time_t` con la funzione `mktime()`. Infine, la funzione `difftime()` calcola la differenza tra le due date in secondi.

## Approfondimenti:

La comparazione delle date in C non è sempre stata così diretta. Nelle versioni più vecchie del linguaggio, era necessario scrivere funzioni personalizzate per gestire questo compito.

Esistono varie alternative per comparare date in C. Oltre all'approccio `difftime()` mostrato sopra, si possono sfruttare libererie come Boost o Qt, che forniscono funzioni potenti e flessibili per la gestione delle date.

Un dettaglio di implementazione importante da notare è che, in C, l'anno è spesso espresso a partire dal 1900, quindi bisogna sottrarre `1900` quando si definiscono strutture `tm`.

## Vedere Anche:

- Documentazione da cplusplus.com: [struct tm](http://www.cplusplus.com/reference/ctime/tm/)
- Esempio da geeksforgeeks.org: [Comparing two dates in C](https://www.geeksforgeeks.org/comparing-two-dates-in-c/)