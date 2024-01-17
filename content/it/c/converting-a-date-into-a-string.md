---
title:                "Convertire una data in una stringa"
html_title:           "C: Convertire una data in una stringa"
simple_title:         "Convertire una data in una stringa"
programming_language: "C"
category:             "C"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/c/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Che cosa è e perché?
Convertire una data in una stringa è un'operazione comune nella programmazione. Consiste nel trasformare una data, rappresentata solitamente in formato numerico o strutturato, in una sequenza di caratteri che ne rappresentino il valore. I programmatori fanno questo per poter visualizzare la data in un formato comprensibile agli utenti o per salvarla in un file di testo.

## Come fare:
```C
#include <stdio.h>
#include <stdlib.h>
#include <time.h>

int main() {
    // Definiamo una data
    time_t data = time(NULL);

    // Convertiamo la data in una stringa
    char *data_stringa = ctime(&data);

    // Stampiamo la stringa
    printf("La data di oggi è: %s", data_stringa);

    return 0;
}
```
**Output:**
La data di oggi è: Wed Aug 25 14:59:56 2021

## Approfondimento:
La necessità di convertire una data in una stringa risale all'era pre-computer, quando le date venivano registrate su carta o su altri supporti fisici. Con l'avvento dei calcolatori, è diventato necessario rappresentare la data in un formato digitale. Oggi, esistono diverse alternative a questa operazione, come ad esempio l'utilizzo di librerie esterne o di funzioni specifiche del linguaggio di programmazione utilizzato. L'implementazione dipende dalla struttura della data e dalle esigenze del programma.

## Vedi anche:
- [La libreria C time.h](https://www.tutorialspoint.com/c_standard_library/time_h.htm)
- [Funzioni di gestione del tempo in C](https://www.geeksforgeeks.org/time-h-header-file-in-c-with-examples/)
- [Come convertire una data in una stringa in C++](https://www.geeksforgeeks.org/converting-strings-numbers-cc/)
- [Come rappresentare le date in SQL](https://www.w3schools.com/sql/sql_dates.asp)