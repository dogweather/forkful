---
title:                "Confronto tra due date"
date:                  2024-01-20T17:32:19.007728-07:00
model:                 gpt-4-1106-preview
simple_title:         "Confronto tra due date"

category:             "C"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/c/comparing-two-dates.md"
---

{{< edit_this_page >}}

## What & Why? (Cosa e Perché?)
Confrontare due date in programmazione significa verificare se una data è precedente, successiva o uguale a un'altra. Questo è fondamentale per gestire prenotazioni, scadenze, eventi storici e qualsiasi logica basata sul tempo.

## How to? (Come fare?)
```C
#include <stdio.h>
#include <time.h>

int main() {
    // Date in formato AAAA-MM-GG (anno-mese-giorno)
    struct tm data1 = {0, 0, 0, 15, 4, 120}; // 15 maggio 2020
    struct tm data2 = {0, 0, 0, 10, 7, 121}; // 10 agosto 2021

    // Converti in tempo UNIX (secondi dal 1 gennaio 1970)
    time_t t1 = mktime(&data1);
    time_t t2 = mktime(&data2);

    // Confronto
    if (difftime(t1, t2) > 0) {
        puts("Data1 è successiva a Data2");
    } else if (difftime(t1, t2) < 0) {
        puts("Data1 è precedente a Data2");
    } else {
        puts("Le date sono uguali");
    }

    return 0;
}

// Output:
// Data1 è precedente a Data2
```

## Deep Dive (Nel Profondo)
Il confronto di date in C richiede di considerare fusi orari, cambiamenti dovuti all'introduzione del calendario Gregoriano, e altri aspetti storici per precisione assoluta. Prima del tipo `time_t` e della funzione `mktime()`, le date venivano spesso gestite come stringhe o numeri, il che poteva essere fonte di errori. Oggi, `time_t` rende il confronto di date meno propenso ad errori. È importante notare che `difftime()` restituisce la differenza in secondi come `double`, sicura per confronti tra date molto lontane.

## See Also (Vedi Anche)
- The C Standard Library: https://en.cppreference.com/w/c/chrono
- GNU C Library - Time Functions: https://www.gnu.org/software/libc/manual/html_node/Time-Functions.html
- Date and Time utilities in C: https://en.cppreference.com/w/c/chrono
