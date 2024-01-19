---
title:                "Analizzare una data da una stringa"
html_title:           "Fish Shell: Analizzare una data da una stringa"
simple_title:         "Analizzare una data da una stringa"
programming_language: "C"
category:             "C"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/c/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

# Analisi di una Data da una Stringa in C

## Cos'è & Perché?

L'interpretazione di una data da una stringa è l'azione di estrarre informazioni sulla data da un formato di stringa. I programmatori lo fanno per processare e manipolare le date in una forma più utilizzabile nel codice.

## Come fare:

Ecco un esempio semplice su come analizzare una data da una stringa usando la funzione `strptime` della libreria `time.h` in C.

```C
#include <time.h>
#include <stdio.h>

int main() {
    struct tm tm;
    char buf[255];

    strptime("2022-02-15 22:45:50", "%Y-%m-%d %H:%M:%S", &tm);
    strftime(buf, sizeof(buf), "%d %B %Y", &tm);
    
    printf("Data analizzata: %s\n", buf);
    
    return 0;
}
```

Uscita:

```
Data analizzata: 15 February 2022
``` 

## Approfondimento

1. **Contesto storico**: L'interpretazione di una data da una stringa è una necessità comune nella programmazione da quando le date sono state rese disponibili in formato di stringa. Le funzioni come `strptime` sono state introdotte per risolvere questo problema.
   
2. **Alternative**: Ci sono diverse librerie disponibili che offrono funzioni di parsing della data come `date.h` o `boost/date_time.hpp` in C++. Queste offrono una gamma più ampia di funzioni e opzioni.

3. **Dettagli Implementativi**: La funzione `strptime` prende una stringa e un formato come input e restituisce una struttura `tm`. Questa struttura contiene dettagli sulla data come anno, mese, giorno, ora, minuto, e secondo.

## Vedi Anche

- [`strptime`](https://pubs.opengroup.org/onlinepubs/007908799/xsh/strptime.html), La Funzione Originale da `time.h`
- [`boost::date_time`](https://www.boost.org/doc/libs/1_76_0/doc/html/date_time.html), Eccezionali Capacità di Parsing della Data in C++
- [`strftime`](https://www.cplusplus.com/reference/ctime/strftime/), Per Formattare la Data in Stringhe