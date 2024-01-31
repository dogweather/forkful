---
title:                "Conversione di una data in una stringa"
date:                  2024-01-20T17:35:52.760372-07:00
model:                 gpt-4-1106-preview
simple_title:         "Conversione di una data in una stringa"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/c/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## What & Why?
Convertire una data in una stringa significa trasformare il formato della data, che di solito è un tipo di dato strutturato, in una sequenza di caratteri leggibile. I programmatori lo fanno per semplificare la visualizzazione e il salvataggio delle date nei file o database e per renderle comprensibili agli utenti.

## How to:
In C useremo `strftime` per convertire struct `tm` in una stringa di data formattata:

```C
#include <stdio.h>
#include <time.h>

int main() {
    time_t now = time(NULL);
    struct tm *tm_struct = localtime(&now);
    char date_string[100];

    strftime(date_string, sizeof(date_string), "%d-%m-%Y %H:%M:%S", tm_struct);
    
    printf("Data formattata: %s\n", date_string);
    
    return 0;
}
```
Output:
```
Data formattata: 31-03-2023 21:45:12
```

## Deep Dive
La conversione delle date in stringhe è un bisogno comune. Prima dell'adozione universale di funzioni standard come `strftime`, ogni sistema operativo aveva il proprio modo per gestire le date, rendendo il codice meno portabile. 
Alternative:
- `sprintf` o `snprintf` possono essere usati per formati semplici ma sono meno flessibili.
- Librerie di terze parti come `Boost` in C++ offrono funzionalità simili con più opzioni.
Dettagli implementativi:
- `strftime` è parte di `<time.h>` e prende un buffer di caratteri, la sua dimensione massima, la stringa di formato e un puntatore a `tm_struct`, convertendo la struttura data in una stringa secondo il formato specificato.

## See Also
- Documentazione di `strftime`: https://en.cppreference.com/w/c/chrono/strftime
- Tutorial sulla gestione del tempo in C: https://www.tutorialspoint.com/c_standard_library/time_h.htm
- `Boost.Date_Time` per C++: https://www.boost.org/doc/libs/1_76_0/doc/html/date_time.html
