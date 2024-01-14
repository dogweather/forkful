---
title:    "C: Calcolare una data nel futuro o nel passato"
keywords: ["C"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/it/c/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

# Perché Calcolare una Data nel Futuro o nel Passato

In questo blog post, esploreremo come calcolare una data nel futuro o nel passato utilizzando il linguaggio di programmazione C. Potrebbe sembrare un'operazione banale, ma in realtà può essere molto utile in molte situazioni, come nella gestione di scadenze, nel calcolo del tempo trascorso o nel previsione di eventi futuri.

# Come Fare

Per calcolare una data nel futuro o nel passato, è prima necessario avere un dato di partenza, cioè una data di riferimento. Questa può essere fornita dall'utente o può essere predefinita dal programma stesso. Successivamente, è necessario determinare il numero di giorni da aggiungere o sottrarre alla data di riferimento per ottenere la nuova data desiderata. Infine, utilizzando funzioni di calcolo date come `mktime` e `localtime`, è possibile ottenere la data definitiva.

Vediamo un esempio pratico di come calcolare una data nel futuro.

```
#include <stdio.h>
#include <time.h>

int main() {

    // definiamo la data di riferimento
    struct tm date = {0};
    date.tm_mday = 7;
    date.tm_mon = 10;
    date.tm_year = 2021 - 1900;

    // numero di giorni da aggiungere alla data di riferimento
    int days = 30;

    // calcoliamo la nuova data
    mktime(&date);
    date.tm_mday += days;
    mktime(&date);

    // stampiamo la nuova data
    printf("La nuova data è: %d/%d/%d\n", date.tm_mday, date.tm_mon + 1, date.tm_year + 1900);

    return 0;
}
```

**Output:**

```
La nuova data è: 6/12/2021
```

# Approfondimento

Possiamo notare che nella libreria `time.h` esistono diverse funzioni per la manipolazione di date, come `mktime` e `localtime`, che ci permettono di convertire una data da formato `struct tm` a formato `time_t` e viceversa.

Inoltre, il calcolo delle date nel futuro o passato può essere reso più complesso aggiungendo ulteriori parametri, come ad esempio considerare anche gli anni bisestili o i fusi orari.

# Vedi Anche

- [Funzioni di data e ora in C](https://www.geeksforgeeks.org/different-date-time-related-structures-c/)
- [Tutorial su come utilizzare la libreria <time.h>](https://www.tutorialspoint.com/c_standard_library/time_h.htm)
- [Calcolo del tempo trascorso in C](https://www.includehelp.com/c-time-library-programming/calculating-time-difference.aspx)