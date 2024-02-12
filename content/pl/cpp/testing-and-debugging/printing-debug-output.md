---
title:                "Drukowanie komunikatów debugowania"
date:                  2024-01-20T17:51:57.399643-07:00
model:                 gpt-4-1106-preview
simple_title:         "Drukowanie komunikatów debugowania"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/cpp/printing-debug-output.md"
---

{{< edit_this_page >}}

## Co i dlaczego?
Drukowanie informacji debug jest jak dziennik programisty. Pomaga śledzić co się dzieje w kodzie — kiedy, gdzie i dlaczego. Programiści używają tej techniki, żeby szybko wykrywać błędy i monitorować zachowanie aplikacji.

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
