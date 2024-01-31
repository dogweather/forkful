---
title:                "Calcolo di una data futura o passata"
date:                  2024-01-20T17:28:36.325353-07:00
model:                 gpt-4-1106-preview
html_title:           "C++: Calcolo di una data futura o passata"
simple_title:         "Calcolo di una data futura o passata"

category:             "C"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/c/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## What & Why?
(Che cosa & Perché?)
Calcolare una data nel futuro o passato in C è semplicemente trovare un'altra data basata su un intervallo di tempo aggiunto a o sottratto da una data di partenza. Programmatori fanno ciò per gestire eventi pianificati, scadenze, e funzionalità di promemoria.

## How to:
(Come fare:)
Il seguente esempio mostra come calcolare una data futura aggiungendo giorni a una data corrente.

```C
#include <stdio.h>
#include <time.h>

int main() {
    struct tm today = {0}, future_date = {0};
    time_t rawtime, future_rawtime;
    
    // Ottieni il tempo corrente e mettilo in today
    time(&rawtime);
    today = *localtime(&rawtime);
    
    // Aggiungi 30 giorni al tempo corrente
    future_date = today;
    future_date.tm_mday += 30;
    
    // Normalizza la struttura tm e converti in tempo Unix
    future_rawtime = mktime(&future_date);
    
    // Converti il futuro tempo Unix in una data leggibile
    future_date = *localtime(&future_rawtime);
    
    // Stampa la data corrente e la data futura
    printf("Data corrente: %02d/%02d/%d\n", today.tm_mday, today.tm_mon + 1, today.tm_year + 1900);
    printf("Data futura (+30 giorni): %02d/%02d/%d\n", future_date.tm_mday, future_date.tm_mon + 1, future_date.tm_year + 1900);

    return 0;
}
```
Output di esempio:
```
Data corrente: 10/04/2023
Data futura (+30 giorni): 10/05/2023
```
## Deep Dive:
(Approfondimento)
Calcolare date è una pratica comune fin dagli albori dell'informatica. Nei primi computer, gestire date e tempo era più complicato a causa delle limitazioni sia hardware che software. Oggi, librerie standard in C come `<time.h>` semplificano queste operazioni, ma possono esserci problemi legati a fusi orari e ora legale.

Un'alternativa è usare `gmtime()` per gestire il tempo UTC, che non considera i fusi orari. In contesti critici, come la programmazione finanziaria o aeronautica, usare librerie esterne come `libdate` può essere più sicuro per evitare errori.

Quando si aggiungono giorni a una data bisogna considerare anni bisestili, i mesi con diversi giorni, e altre complessità. La funzione `mktime()` gestisce automaticamente questi problemi normalizzando la struttura `tm`.

## See Also:
(Vedi anche)
- C Standard Library: `<time.h>` (https://en.cppreference.com/w/c/chrono)
- POSIX `strftime` per formattare date e tempi (https://en.cppreference.com/w/c/chrono/strftime)
- Howard Hinnant's date library per operazioni di data avanzate (https://github.com/HowardHinnant/date)
