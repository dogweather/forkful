---
title:                "Ottenere la data corrente"
html_title:           "C++: Ottenere la data corrente"
simple_title:         "Ottenere la data corrente"
programming_language: "C++"
category:             "C++"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/cpp/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Perché

Molte volte come programmatori ci troveremo nella situazione in cui abbiamo bisogno di ottenere la data corrente all'interno del nostro codice, per svolgere determinate operazioni o per mostrare informazioni aggiornate all'utente. Questo può essere fatto facilmente utilizzando il linguaggio di programmazione C++.

## Come Fare

Per ottenere la data corrente in C++, esistono alcune funzioni disponibili all'interno della libreria standard del linguaggio. La più comune è la funzione `time()` che restituisce il numero di secondi trascorsi dal 1 gennaio 1970. Utilizzando questa funzione all'interno di una variabile di tipo `time_t` possiamo poi utilizzare la funzione `localtime()` per ottenere la data corrente in un formato più leggibile.

Ecco un esempio di codice che mostra come ottenere e stampare la data corrente nel formato DD/MM/AAAA:

```C++
#include <iostream>
#include <ctime>

int main() {
    // Otteniamo i secondi trascorsi dal 1/1/1970
    time_t now = time(0);

    // Lo convertiamo in una data più leggibile
    struct tm* currentDate = localtime(&now);

    // Otteniamo le informazioni di data dal currentDate
    int day = currentDate->tm_mday;         // giorno (da 1 a 31)
    int month = currentDate->tm_mon + 1;    // mese (da 0 a 11)
    int year = currentDate->tm_year + 1900; // anno (da 1900)

    // Stampiamo la data nel formato DD/MM/AAAA
    std::cout << day << "/" << month << "/" << year;

    return 0;
}
```

L'output del codice sarà la data corrente nel formato DD/MM/AAAA. Ad esempio, se il codice viene eseguito il 17/02/2021, l'output sarà:

```
17/02/2021
```

## Approfondimento

Oltre alla funzione `localtime()`, esistono altre funzioni della libreria standard di C++ che possono essere utilizzate per ottenere la data e l'ora corrente in diversi formati. Ad esempio, la funzione `strftime()` permette di ottenere la data nel formato desiderato, passando come argomenti il formato desiderato e la struttura che contiene le informazioni di data e ora.

Inoltre, esistono anche librerie esterne che offrono funzionalità più avanzate per la gestione della data, come ad esempio la libreria Boost.Date_Time.

## Vedi Anche

- [Documentazione ufficiale di C++ sulla libreria `ctime`](https://en.cppreference.com/w/cpp/header/ctime)
- [Tutorial su come lavorare con le date in C++](https://www.learncpp.com/cpp-tutorial/511-arithmetic-with-dates-and-times/)