---
date: 2024-01-20 17:51:57.399643-07:00
description: "Drukowanie informacji debug jest jak dziennik programisty. Pomaga \u015B\
  ledzi\u0107 co si\u0119 dzieje w kodzie \u2014 kiedy, gdzie i dlaczego. Programi\u015B\
  ci u\u017Cywaj\u0105 tej\u2026"
lastmod: '2024-03-13T22:44:35.715522-06:00'
model: gpt-4-1106-preview
summary: Drukowanie informacji debug jest jak dziennik programisty.
title: "Drukowanie komunikat\xF3w debugowania"
weight: 33
---

## Jak to zrobić:
```C++
#include <iostream>

int main() {
    // Prosty output do konsoli
    std::cout << "Start aplikacji" << std::endl;

    int liczba = 42;
    // Debug wartości zmiennej
    std::cerr << "Wartość zmiennej liczba: " << liczba << std::endl;

    // ... reszta programu

    std::cout << "Koniec aplikacji" << std::endl;
    return 0;
}
```
Output:
```
Start aplikacji
Wartość zmiennej liczba: 42
Koniec aplikacji
```

## Szczegółowo:
W dawnych czasach programiści debugowali kod na kartach perforowanych. Dzisiaj mamy `std::cout` i `std::cerr` – standardowe strumienie w C++. Używamy `std::cout` do wydruku normalnych danych, a `std::cerr` do komunikatów błędów czy debugu, który może być przekierowany do innego miejsca niż główny output programu.

Alternatywy to np. używanie bibliotek jak `log4cpp` czy `spdlog`, które oferują zaawansowane opcje logowania. Do wypisywania w trybie debug mogą też służyć makra, jak `assert`, które służą do weryfikacji założeń w kodzie.

## Zobacz także:
- [cppreference.com: Input/output library](https://en.cppreference.com/w/cpp/io)
- [github.com: spdlog](https://github.com/gabime/spdlog)
