---
title:                "C++: Ottenere la data corrente"
programming_language: "C++"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/cpp/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Perché

Imparare come ottenere la data corrente è fondamentale per qualsiasi programmatore C++. Essendo una delle lingue di programmazione più popolari al mondo, è importante avere familiarità con le funzionalità di base come questa.

## Come Fare

Per ottenere la data corrente in C++, dobbiamo usare la libreria `ctime` e la funzione `time()` per ottenere l'ora corrente in secondi dall'1 gennaio 1970. Quindi, utilizzando `localtime()` possiamo convertire questa informazione in una struttura `tm` contenente la data e l'ora attuali. Ecco un esempio di codice che otterrà e stamperà la data corrente:

```C++
#include <iostream>
#include <ctime>

int main() {
    // Ottenere l'ora corrente come intero
    time_t current_time = time(0);

    // Convertire l'ora in una struttura tm
    tm *now = localtime(&current_time);

    // Stampare la data corrente utilizzando le funzioni della struttura tm
    std::cout << "Data corrente: " << now->tm_mday << "/" << now->tm_mon + 1 << "/" << now->tm_year + 1900 << std::endl;
}
```

Output:

```
Data corrente: 27/11/2021
```

## Approfondimento

È importante notare che la funzione `localtime()` utilizza il fuso orario del sistema in cui viene eseguita, quindi la data corrente potrebbe variare se si esegue il programma in un computer con fuso orario diverso. Inoltre, la struttura `tm` ha diverse funzioni utili per ottenere l'ora o eventuali altre informazioni sulla data corrente.

## Vedi Anche

- [Documentazione ufficiale di C++ per la libreria ctime](https://en.cppreference.com/w/cpp/header/ctime)
- [Tutorial su come utilizzare la libreria ctime in C++](https://www.programiz.com/cpp-programming/library-function/ctime)
- [Guida su come utilizzare le funzioni della struttura tm in C++](https://www.geeksforgeeks.org/c-real-numbers-and-date-time-functions-examples-of-clock-function-localtime-and-time/)