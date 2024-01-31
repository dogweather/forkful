---
title:                "Ottenere la data corrente"
date:                  2024-01-20T15:13:09.803019-07:00
html_title:           "Arduino: Ottenere la data corrente"
simple_title:         "Ottenere la data corrente"

category:             "C++"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/cpp/getting-the-current-date.md"
---

{{< edit_this_page >}}

## What & Why? (Cosa & Perché?)
Ottenere la data corrente in C++ significa catturare la data di oggi dal sistema. I programmatori lo fanno per log, timestamp o funzionalità date-dipendenti.

## How to: (Come fare:)
```C++
#include <iostream>
#include <chrono>
#include <ctime>

int main() {
    // Get the current date/time based on the system clock
    auto now = std::chrono::system_clock::now();
    std::time_t now_time = std::chrono::system_clock::to_time_t(now);

    // Convert now_time to string form
    std::string current_time_str = std::ctime(&now_time);

    // Output the current date and time
    std::cout << "Data e ora correnti: " << current_time_str;

    return 0;
}
```
Output:
```
Data e ora correnti: Wed Mar 10 12:33:45 2023
```

## Deep Dive (Approfondimento)
Fino a C++11, `<ctime>` era l'opzione standard per la data/ora. Con C++11, `<chrono>` è il nuovo modulo per la misura del tempo. `<chrono>` offre precisione e funzionalità superiori. Le alternative includono librerie di terze parti come Boost.DateTime. L'uso di `<chrono>` permette funzionalità portabili e thread-safe per il recupero della data e ora attuali.

## See Also (Vedi Anche)
- Documentazione di C++ `<chrono>`: https://en.cppreference.com/w/cpp/chrono
- Introduzione a `<ctime>`: https://en.cppreference.com/w/cpp/header/ctime
- Libreria Boost.DateTime: https://www.boost.org/doc/libs/release/libs/date_time/
- Tutorial su `<chrono>`: https://www.learncpp.com/cpp-tutorial/8-5-stdchronotime_point/
