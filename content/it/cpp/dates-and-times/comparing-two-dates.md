---
date: 2024-01-20 17:32:19.222161-07:00
description: "Comparare due date permette di stabilire quale avviene prima o dopo.\
  \ \xC8 essenziale in applicazioni come prenotazioni, scadenze e timeline di eventi."
lastmod: '2024-03-13T22:44:43.739934-06:00'
model: gpt-4-1106-preview
summary: Comparare due date permette di stabilire quale avviene prima o dopo.
title: Confronto tra due date
weight: 27
---

## How to:
C++20 introduce `<chrono>` con facilmente usabili `year_month_day`, che rende il confronto di date chiaro.

```C++

#include <iostream>
#include <chrono>
#include <format>

using namespace std;
using namespace std::chrono;

int main() {
    year_month_day data1 = 2023y/3/15;
    year_month_day data2 = 2023y/6/10;

    if (data1 < data2) {
        cout << format("La data {} viene prima di {}.\n", data1, data2);
    } else if (data1 > data2) {
        cout << format("La data {} viene dopo {}.\n", data1, data2);
    } else {
        cout << "Le date sono uguali.\n";
    }

    return 0;
}
```
Output:
```
La data 2023-03-15 viene prima di 2023-06-10.
```

## Deep Dive:
Comparare date è un problema old as time (letteralmente). Prima di C++20, avremmo usato `<ctime>` o librerie esterne come Boost.DateTime. Ma ora, `<chrono>` ci dà gli strumenti per gestire tempo e date nel moderno C++.

Le alternative a `<chrono>` includono la vecchia `<ctime>` o librerie di terze parti, ma `<chrono>` vince per pulizia e integrazione con il linguaggio.

Il confronto tra `year_month_day` è supportato naturalmente grazie a operatori logici. È ben definito e non dovrai gestire conversioni o calcoli manuali. Fai solo attenzione a fusi orari e calendari diversi che `<chrono>` non gestisce nativamente.

## See Also:
C++ Reference su `<chrono>`: https://en.cppreference.com/w/cpp/chrono
Tutorial su `<chrono>`: https://www.modernescpp.com/index.php/c-20-the-calendar-and-time-zones
