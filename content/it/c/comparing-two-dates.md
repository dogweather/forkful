---
title:                "Confrontare due date"
html_title:           "C: Confrontare due date"
simple_title:         "Confrontare due date"
programming_language: "C"
category:             "C"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/c/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Cos'è e perché?

Comparare due date è l'azione di confrontare due date per determinare quale sia la maggiore o la minore delle due. I programmatori spesso lo fanno quando devono ordinare una lista di eventi temporali o quando devono implementare funzionalità per il confronto di date in un programma.

## Come fare:

```C
#include <stdio.h>
#include <time.h>

int main(void) {
    // Definisce le due date da confrontare
    struct tm date1 = { .tm_year = 121, .tm_mon = 6, .tm_mday = 25 }; // 25 Luglio 2021
    struct tm date2 = { .tm_year = 121, .tm_mon = 10, .tm_mday = 11 }; // 11 Novembre 2021

    // Converte le date in secondi
    time_t seconds1 = mktime(&date1);
    time_t seconds2 = mktime(&date2);

    // Confronta le date e stampa il risultato
    if (seconds1 < seconds2) {
        printf("La prima data è antecedente alla seconda data.");
    } else if (seconds1 == seconds2) {
        printf("Le due date sono uguali.");
    } else {
        printf("La prima data è successiva alla seconda data.");
    }

    return 0;
}
```

Sample output:
```
La prima data è antecedente alla seconda data.
```

## Approfondimento:

- Contesto storico:
La data è uno dei concetti più antichi nella storia dell'umanità e i primi tentativi di creare un sistema per organizzarle risalgono all'antica civiltà egizia circa 5000 anni fa. Nel mondo della programmazione, i primi linguaggi come ALGOL, FORTRAN e COBOL non disponevano di una struttura dati nativa per gestire le date, rendendo questa operazione piuttosto complessa.

- Alternative:
Esistono diverse librerie di terze parti, come ad esempio "libdate" e "datetime.h", che offrono funzionalità più avanzate per la manipolazione e il confronto di date, oltre a gestire diverse fusi orari.

- Dettagli di implementazione:
La funzione `mktime()` è utilizzata per convertire una data nella sua rappresentazione in secondi dal 1 gennaio 1970, nota come "epoch time". Inoltre, la struttura `tm` è utilizzata per rappresentare una data, in particolare il suo campo `tm_year` indica l'anno a partire dal 1900 e il campo `tm_mon` indica il numero del mese (1-12). Per maggiori dettagli, è possibile consultare la documentazione ufficiale.

## Vedi anche:

- [Funzioni per la manipolazione delle date in C](https://www.geeksforgeeks.org/c-function-to-leap-year/)
- [Libreria libdate](https://sourceforge.net/projects/libdate/)
- [Libreria datetime.h](https://github.com/realtazy/datetime.h)