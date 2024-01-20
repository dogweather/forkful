---
title:                "Calcolare una data nel futuro o nel passato"
html_title:           "C: Calcolare una data nel futuro o nel passato"
simple_title:         "Calcolare una data nel futuro o nel passato"
programming_language: "C"
category:             "C"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/c/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

# Calcolare una data nel futuro o nel passato in C
**Calcolare una data nel futuro o nel passato**: un'operazione comune nelle applicazioni software che coinvolgono la gestione del tempo, come calendari, promemoria, o pianificatori. Consente ai programmatori di determinare un periodo di tempo relativo a una data specifica.

## Come si fa
Per calcolare una data nel futuro o nel passato, possiamo utilizzare la funzione `mktime` e la struttura `struct tm` in C. Nota che puoi aggiungere o sottrarre giorni come desideri. Ecco un esempio:

```C
#include <stdio.h>
#include <time.h>

int main() {
    time_t now;
    struct tm new_date;

    time(&now);
    new_date = *localtime(&now);

    new_date.tm_mday += 5;
    mktime(&new_date);

    printf("La data tra 5 giorni sarà %s", asctime(&new_date));

    return 0;
}
```

In questo esempio, stiamo calcolando la data che sarà tra 5 giorni. Il risultato sarà stampato in formato: `gio   mese   giorno   ora   anno\n`

## Approfondimento
Le funzioni `time`, `localtime`, `mktime` e la struttura `struct tm` derivano dai primi giorni del linguaggio di programmazione C. Queste funzioni e strutture sono state la soluzione per gestire il tempo poiché UNIX, uno dei primi sistemi operativi su cui C è stato largamente utilizzato, ha iniziato a diventare popolare.

Molte alternative sono disponibili per calcolare una data futura o passata, come l'uso di librerie di terze parti come `<date.h>` di Howard Hinnant o tecniche aritmetiche per calcolare il giorno successivo. Tuttavia, l'approccio sopra menzionato è generalmente adottato per la sua semplicità e compatibilità con il C standard.

## Per Saperne di Più
Per saperne di più sul calcolo delle date future e passate e sulla gestione del tempo in generale in C, alcuni link utili includono:

- [The GNU C Library: Date and Time](https://www.gnu.org/software/libc/manual/html_node/Date-and-Time.html)
- [C Programming - Date and Time, Tutorials Point](https://www.tutorialspoint.com/c_standard_library/c_function_localtime.htm)
- [Time library functions in C++](https://www.geeksforgeeks.org/time-library-c-set-1/)