---
title:                "Convertire una data in una stringa"
html_title:           "Javascript: Convertire una data in una stringa"
simple_title:         "Convertire una data in una stringa"
programming_language: "C"
category:             "C"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/c/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Cos'è & Perché?

Convertire una data in una stringa significa rappresentare un'informazione temporale in un formato leggibile dall'uomo. Questa operazione è utile per visualizzare date nell'interfaccia utente o per memorizzare informazioni temporali in file di testo.

## Come fare:

Per convertire una data in una stringa in C, utilizziamo la funzione `strftime` della libreria standard. Ecco un esempio:

```C
#include <time.h>
#include <stdio.h>

int main() {
    char dataStr[100];
    time_t t = time(NULL);
    struct tm tm = *localtime(&t);
    
    strftime(dataStr, sizeof(dataStr), "%d/%m/%Y %H:%M:%S", &tm);
    printf("Data e ora corrente: %s\n", dataStr);
    
    return 0;
}
```
Nel tuo output vedrai qualcosa di simile a `Data e ora corrente: 01/01/2022 12:01:01`.

## Approfondimento

La funzione `strftime` è stata introdotta nel linguaggio C da ANSI C nel 1989. È una funzione molto versatile che permette diverse combinazioni di formati.

Un'alternativa a `strftime` potrebbe essere la formattazione manuale delle componenti della data, ma `strftime` è molto più semplice da usare e consente di gestire facilmente diversi formati.

Un dettaglio importante della funzione `strftime` è che ritorna il numero di caratteri scritti nella stringa di destinazione. Se lo spazio non è sufficiente, gli ultimi caratteri verranno troncati.

## Vedi Anche

1. Documentazione della funzione strftime: http://www.cplusplus.com/reference/ctime/strftime/
2. Altri esempi di utilizzo: https://www.programiz.com/c-programming/library-function/time.h/strftime
3. Time library in C: https://www.tutorialspoint.com/c_standard_library/time_h.htm