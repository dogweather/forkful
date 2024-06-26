---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:09:08.832595-07:00
description: "Come fare: C++ offre diversi modi per ottenere la data corrente, inclusa\
  \ la libreria standard di C++ e librerie di terze parti come Boost. Gli esempi\u2026"
lastmod: '2024-03-13T22:44:43.737445-06:00'
model: gpt-4-0125-preview
summary: C++ offre diversi modi per ottenere la data corrente, inclusa la libreria
  standard di C++ e librerie di terze parti come Boost.
title: Ottenere la data corrente
weight: 29
---

## Come fare:
C++ offre diversi modi per ottenere la data corrente, inclusa la libreria standard di C++ e librerie di terze parti come Boost. Gli esempi seguenti dimostrano come realizzare questo compito.

### Utilizzando `<chrono>` (C++20 e successivi)
C++20 ha introdotto maggiori funzionalità nella libreria `<chrono>`, rendendo semplice ottenere la data corrente:
```cpp
#include <iostream>
#include <chrono>
#include <format> // Per std::format (C++20)

int main() {
    auto current_time_point = std::chrono::system_clock::now(); // Cattura il tempo corrente
    auto current_time_t = std::chrono::system_clock::to_time_t(current_time_point); // Converti in time_t

    // Formatta il tempo in un formato leggibile
    std::cout << "Data Corrente: " << std::format("{:%Y-%m-%d}", std::chrono::system_clock::to_time_t(current_time_point)) << std::endl;

    return 0;
}
```
**Output di Esempio:**
```plaintext
Data Corrente: 2023-03-15
```

### Utilizzando `<ctime>`
Per i programmatori che lavorano con versioni precedenti di C++ o per coloro che preferiscono la libreria C tradizionale:
```cpp
#include <iostream>
#include <ctime>

int main() {
    std::time_t t = std::time(0); // Ottieni il tempo corrente
    std::tm* now = std::localtime(&t);
    std::cout << "Data Corrente: " 
              << (now->tm_year + 1900) << '-' 
              << (now->tm_mon + 1) << '-'
              <<  now->tm_mday
              << std::endl;

    return 0;
}
```
**Output di Esempio:**
```plaintext
Data Corrente: 2023-03-15
```

### Utilizzando Boost Date_Time
Per i progetti che utilizzano le librerie Boost, la libreria Boost Date_Time offre un metodo alternativo per ottenere la data corrente:
```cpp
#include <iostream>
#include <boost/date_time.hpp>

int main() {
    // Ottieni il giorno corrente usando il calendario gregoriano di Boost
    boost::gregorian::date today = boost::gregorian::day_clock::local_day();
    std::cout << "Data Corrente: " << today << std::endl;

    return 0;
}
```
**Output di Esempio:**
```plaintext
Data Corrente: 2023-Mar-15
```
Questi esempi forniscono una base fondamentale per lavorare con le date in C++, cruciale per una vasta gamma di applicazioni.
