---
title:    "C: Confronto tra due date"
keywords: ["C"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/it/c/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Perché

Comparare due date potrebbe sembrare un'operazione banale, ma in realtà è un aspetto importante nella programmazione. Sia che si stiano sviluppando applicazioni per il web o per dispositivi mobili, spesso si ha la necessità di confrontare date per valutare eventi passati o pianificare attività future. In questo articolo, vedremo come effettuare questo confronto in modo efficace utilizzando il linguaggio di programmazione C.

## Come Fare

Per confrontare due date in C, è necessario utilizzare alcune funzioni fornite dalla libreria standard del linguaggio. In particolare, le funzioni `gmtime()` e `mktime()` saranno fondamentali per ottenere le informazioni necessarie dalle date.

```C
#include <stdio.h>
#include <time.h>

int main() {
    // Definizione delle due date da confrontare
    struct tm date1 = { .tm_year=120, .tm_mon=3, .tm_mday=15 };
    struct tm date2 = { .tm_year=120, .tm_mon=6, .tm_mday=1 };

    // Convertiamo le date in timestamp
    time_t time1 = mktime(&date1);
    time_t time2 = mktime(&date2);

    // Confrontiamo i timestamp ottenuti
    if (time1 > time2) {
        printf("La prima data è più recente della seconda\n");
    } else if (time1 < time2) {
        printf("La seconda data è più recente della prima\n");
    } else {
        printf("Le due date sono identiche\n");
    }

    return 0;
}
```

L'output di questo programma sarà:

```
La seconda data è più recente della prima
```

La funzione `mktime()` restituisce il numero di secondi trascorsi dal 1 ge