---
title:                "Ottenere la data corrente"
date:                  2024-01-20T15:12:58.974550-07:00
html_title:           "Arduino: Ottenere la data corrente"
simple_title:         "Ottenere la data corrente"
programming_language: "C"
category:             "C"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/c/getting-the-current-date.md"
---

{{< edit_this_page >}}

## What & Why?
Ottenere la data corrente significa avere il giorno, mese e anno in cui si esegue il programma. I programmatori lo fanno per log, timestamp e funzionalità temporali.

## How to:
Per ottenere la data corrente in C, usi la libreria `<time.h>`. Ecco un esempio pratico.

```C
#include <stdio.h>
#include <time.h>

int main() {
    time_t rawtime;
    struct tm * timeinfo;

    time(&rawtime);
    timeinfo = localtime(&rawtime);

    printf("Data corrente: %s", asctime(timeinfo));
    return 0;
}
```

Output di esempio:
```
Data corrente: Wed Feb 23 14:22:35 2023
```

## Deep Dive:
Ottenere la data corrente è un'esigenza che va indietro fin agli inizi della programmazione. Negli anni '70, funzioni come `asctime()` e `localtime()` erano già presenti nel linguaggio C. Oggi ci sono alternative come `strftime()` per formattare la data.

Un'occhiata agli elementi:
- `time_t rawtime;` crea una variabile per memorizzare il tempo non formattato.
- `time(&rawtime);` ottiene il tempo attuale.
- `localtime(&rawtime);` converte `time_t` in una struttura leggibile.
- `asctime(timeinfo);` trasforma la struttura in una stringa.

Funzioni come `gmtime()` permettono di ottenere l'UTC anziché l'orario locale. Importante è anche gestire i fusi orari e le configurazioni locali che cambiano da sistema a sistema.

## See Also:
- Manuale di C su `<time.h>`: http://www.cplusplus.com/reference/ctime/
- Approfondimenti sulla `strftime()`: https://en.cppreference.com/w/c/chrono/strftime
- Documentazione GNU su `localtime()`: https://www.gnu.org/software/libc/manual/html_node/Broken_002ddown-Time.html
