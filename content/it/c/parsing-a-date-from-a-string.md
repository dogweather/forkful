---
title:                "Estrarre una data da una stringa"
date:                  2024-01-20T15:34:44.400270-07:00
html_title:           "Arduino: Estrarre una data da una stringa"
simple_title:         "Estrarre una data da una stringa"
programming_language: "C"
category:             "C"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/c/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## What & Why?
Tradurre una data da una stringa significa convertire un testo in una struttura data che il programma può capire e usare. I programmatori lo fanno per poter manipolare e confrontare date, spesso inserite dagli utenti o lette da file.

## How to:
Per estrarre una data da una stringa in C, possiamo usare la funzione `strptime`, disponibile nella libreria time.h. Ecco un esempio pratico:

```C
#include <stdio.h>
#include <time.h>

int main() {
    struct tm tm;
    char *str = "01/04/2023";
    strptime(str, "%d/%m/%Y", &tm);

    printf("Giorno: %d, Mese: %d, Anno: %d\n", tm.tm_mday, tm.tm_mon + 1, tm.tm_year + 1900);
    
    return 0;
}
```

Output:
```
Giorno: 1, Mese: 4, Anno: 2023
```

## Deep Dive
La funzione `strptime` è stata introdotta in POSIX.1-2001. Nota che non è parte dello standard C ISO, quindi alcuni compilatori Windows potrebbero non supportarla. Alternative includono la funzione `sscanf` o le librerie di terze parti come 'date.h'.

La scelta di `strptime` è dovuta alla sua capacità di interpretare diversi formati di date, e alla sua integrazione naturale con le strutture `tm` di time.h. La funzione legge la data dalla stringa secondo il formato specificato dal programmatore, e popola una struttura `tm` con anno, mese, giorno ecc.

Va ricordato che `tm_mon` inizia da 0 per gennaio e `tm_year` è l’anno meno 1900, quindi bisogna regolare questi valori per l’output.

## See Also
- Documentazione POSIX su `strptime`: https://pubs.opengroup.org/onlinepubs/9699919799/functions/strptime.html
- Libreria 'date.h' per un handling più robusto delle date in C++: https://github.com/HowardHinnant/date
- Tutorial su `struct tm` e gestione del tempo in C: https://www.cplusplus.com/reference/ctime/tm/