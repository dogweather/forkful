---
title:                "Estrarre una data da una stringa"
date:                  2024-01-20T15:35:12.350305-07:00
simple_title:         "Estrarre una data da una stringa"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/cpp/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## What & Why?
(E cosa & Perché?)
Il "parsing" di una data significa trasformarla da stringa a una struttura che il programma può manipolare. Lo facciamo per elaborare, validare e confrontare le date in maniera efficiente.

## How to:
(Come fare:)
```C++
#include <iostream>
#include <sstream>
#include <iomanip>
#include <ctime>

int main() {
    std::string date_str = "20/04/2023";
    std::tm tm = {};
    std::istringstream ss(date_str);
    ss >> std::get_time(&tm, "%d/%m/%Y");  // Formato europeo per le date DD/MM/YYYY

    if (ss.fail()) {
        std::cout << "Parsing fallito!";
    } else {
        std::cout << "Giorno: " << tm.tm_mday << ", Mese: " << (tm.tm_mon + 1) << ", Anno: " << (1900 + tm.tm_year);
    }

    return 0;
}
```
Output:
```
Giorno: 20, Mese: 4, Anno: 2023
```

## Deep Dive:
(Analisi Approfondita:)
Il parsing di date da stringhe è una pratica antica, nata insieme alla necessità di leggere e scrivere dati in forma umanamente leggibile. C++ ha visto varie librerie nel tempo per questa operazione: `<ctime>` è una di quelle, diretta discendente del C standard. Alternativamente, la libreria `<chrono>` più moderna di C++ (introdotto con C++11 e migliorato successivamente) offre approcci più robusti ed estensibili. Per implementare il parsing, `<iomanip>` fornisce funzionalità per la manipolazione dei flussi di input/output, come `std::get_time`, per interpretare stringhe come date.

## See Also:
(Vedi Anche:)

- Documentazione di `<chrono>`: https://en.cppreference.com/w/cpp/header/chrono
- Documentazione di `<iomanip>`: https://en.cppreference.com/w/cpp/header/iomanip
- Esempi di parsing di date con `<chrono>`: https://en.cppreference.com/w/cpp/io/manip/get_time
