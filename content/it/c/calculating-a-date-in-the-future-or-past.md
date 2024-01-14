---
title:                "C: Calcolare una data nel futuro o nel passato"
simple_title:         "Calcolare una data nel futuro o nel passato"
programming_language: "C"
category:             "C"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/c/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Perché

Calcolare una data nel futuro o nel passato può essere utile per molteplici ragioni, come ad esempio la pianificazione di eventi o la gestione di scadenze. Inoltre, può essere un esercizio interessante per sviluppare le proprie abilità di programmazione.

## Come Fare

Per poter calcolare correttamente una data nel futuro o nel passato, è necessario avere a disposizione alcune informazioni fondamentali, come la data di partenza e il numero di giorni da aggiungere o sottrarre. Usando il linguaggio di programmazione C, possiamo utilizzare la libreria standard `time.h` per manipolare le date. Di seguito è riportato un esempio di codice che calcola la data di domani rispetto alla data attuale:

```C
#include <stdio.h>
#include <time.h>

int main()
{
    // Otteniamo la data attuale
    time_t t = time(NULL);
    struct tm *now = localtime(&t);

    // Aggiungiamo un giorno
    now->tm_mday += 1;
    mktime(now);

    // Stampiamo la data di domani nel formato GG/MM/AAAA
    printf("La data di domani è: %02d/%02d/%d\n", now->tm_mday, now->tm_mon + 1, now->tm_year + 1900);

    return 0;
}
```

Output:

```
La data di domani è: 01/09/2021
```

## Approfondimento

La libreria `time.h` ci permette di manipolare le date in vari modi. Ad esempio, possiamo anche sottrarre giorni o mesi, non solo aggiungerli. Inoltre, ci sono funzioni specializzate per calcolare la differenza tra due date, o per confrontare date diverse. È importante tenere conto della diversa rappresentazione dei mesi (da 0 a 11) e dei giorni (da 1 a 31) all'interno della struttura `tm`. Un ulteriore aspetto importante è la gestione dei casi di anni bisestili, che possono influenzare il calcolo della data.

## Vedi Anche

- [Documentazione ufficiale di `time.h` in italiano](https://pubs.opengroup.org/onlinepubs/7908799/xsh/time.h.html)
- [Esempi di calcolo delle date in C](https://www.includehelp.com/c/date-and-time-programming-in-c.aspx)
- [Tutorial su come gestire le date in C](https://www.tutorialspoint.com/cprogramming/c_date_time.htm)