---
title:                "Analizzare una data da una stringa"
html_title:           "Fish Shell: Analizzare una data da una stringa"
simple_title:         "Analizzare una data da una stringa"
programming_language: "C++"
category:             "C++"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/cpp/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## Cos'è & Perché?

"Analizzare una data da una stringa" significa estrarre e interpretare una data da un testo. I programmatori lo fanno per convertire i dati di ingresso testuali in un formato gestibile da usare nelle elaborazioni.

## Come fare:

Ecco un modo semplice per analizzare una data da una stringa in C++:

```C++
#include<iostream>
#include<sstream>
#include<ctime>

int main(){
    struct std::tm tm;
    std::istringstream ss("2001-10-18");
    ss >> std::get_time(&tm, "%Y-%m-%d");

    if (ss.fail()) {
        std::cout << "Parsing failed\n";
    } else {
        std::cout << std::put_time(&tm, "%c") << "\n";
    }

    return 0;
}   
```   
L'output sarà: Gio Ott 18 00:00:00 2001


## Approfondimenti:

Le funzioni `get_time` e `put_time` sono incorporate in C++ dal 2011, prima si usavano altre funzioni meno pratiche. Esistono alternative alla libreria standard, come la libreria `boost`, ma la via standard è solitamente sufficiente per la maggior parte delle applicazioni.

Un dettaglio implementativo importante riguarda il formato di input. Nell'esempio sopra, abbiamo usato "%Y-%m-%d" che corrisponde a un formato data comune: anno-mese-giorno. È necessario che la stringa di input sia conforme a questo formato.

## Vedi Anche:

- [get_time](https://en.cppreference.com/w/cpp/io/manip/get_time): Documentazione ufficiale sul metodo `get_time`.
- [put_time](https://en.cppreference.com/w/cpp/io/manip/put_time): Documentazione ufficiale sul metodo `put_time`.
- [Boost Date_Time](https://www.boost.org/doc/libs/1_75_0/doc/html/date_time.html): Un'estesa libreria di manipolazione data e ora.