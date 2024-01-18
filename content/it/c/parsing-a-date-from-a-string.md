---
title:                "Parsing di una data da una stringa"
html_title:           "C: Parsing di una data da una stringa"
simple_title:         "Parsing di una data da una stringa"
programming_language: "C"
category:             "C"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/c/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## Che cos'è e perché?
"Analizzare una data da una stringa" è il processo di estrapolare la data da una stringa di testo. I programmatori spesso lo fanno per convertire una data in un formato diverso o per verificarne la validità.

## Come fare:
```C
//Esempio di analisi di una data da una stringa nel formato "dd/mm/aaaa"
#include <stdio.h>
#include <stdlib.h>
#include <time.h>

int main()
{
    char date_str[] = "15/07/2021"; //stringa contenente la data
    struct tm date; //struttura per contenere la data
    //Analisi della stringa usando la funzione sscanf()
    sscanf(date_str,"%d/%d/%d",&date.tm_mday,&date.tm_mon,&date.tm_year);
    //Stampa della data nel formato "mm/dd/yy"
    printf("%02d/%02d/%02d",date.tm_mon,date.tm_mday,date.tm_year);
    return 0;
}
```
Output: 07/15/21

## Approfondimento:
Il parsing di una data da una stringa è una pratica comune nella programmazione ed è utilizzato in molteplici contesti, come ad esempio nei sistemi di prenotazione e nei database. Esistono alcune alternative per gestire questa operazione, come l'utilizzo di librerie esterne o la creazione di funzioni personalizzate. Inoltre, il processo di parsing può essere più complesso se si vogliono gestire formati di data diversi.

## Vedi anche:
- Manuale di C: <https://www.codingunit.com/c-tutorial-date-time-function/>
- Libreria di parsing di data esterna: <https://github.com/HowardHinnant/date>
- Tutorial su come creare una funzione di parsing personalizzata: <https://www.geeksforgeeks.org/how-to-parse-a-date-string-in-c/>