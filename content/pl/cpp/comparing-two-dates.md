---
title:                "C++: Porównywanie dwóch dat"
simple_title:         "Porównywanie dwóch dat"
programming_language: "C++"
category:             "C++"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/cpp/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Dlaczego

Porównywanie dwóch dat jest nieodłącznym elementem wielu programów. Dzięki temu można sprawdzić porządek zdarzeń, weryfikować ważność dokumentów czy szybko obliczyć różnicę między datami. Zrozumienie sposobu porównywania dat jest niezbędne dla każdego programisty.

## Jak to zrobić

W celu porównania dwóch dat w języku C++ można wykorzystać klasę `std::chrono::time_point`. Klasa ta reprezentuje określoną chwilę w czasie i umożliwia porównywanie jej z innymi chwilami. Przykładowy kod wyglądałby następująco:

```C++
#include <iostream>
#include <chrono>

int main() {
  // Tworzenie pierwszej daty
  std::chrono::time_point<std::chrono::system_clock> first_date = std::chrono::system_clock::now();

  // Tworzenie drugiej daty
  std::chrono::time_point<std::chrono::system_clock> second_date = std::chrono::system_clock::now();

  // Porównywanie dat i wyświetlenie wyniku
  if (first_date < second_date) {
    std::cout << "Pierwsza data jest wcześniejsza od drugiej.";
  } else if (first_date > second_date) {
    std::cout << "Druga data jest wcześniejsza od pierwszej.";
  } else {
    std::cout << "Daty są takie same.";
  }

  return 0;
}
```

Przykładowy wynik dla dwóch przypadkowych dat:

```
Pierwsza data jest wcześniejsza od drugiej.
```

## Głębszy wgląd

Klasa `std::chrono::time_point` wykorzystuje reprezentację `std::chrono::duration`, która jest duracją między początkiem epoki czasu (1 stycznia 1970 roku) a daną chwilą. Innymi słowy, czas jest mierzony w odniesieniu do ustalonego punktu czasu. Klasa ta oferuje również funkcje konwersji między różnymi jednostkami czasu, co może być przydatne w niektórych przypadkach.

## Zobacz także

- Dokumentacja klasy `std::chrono::time_point`: https://en.cppreference.com/w/cpp/chrono/time_point
- Tutorial o porównywaniu dat w C++: https://www.learncpp.com/cpp-tutorial/comparing-and-ordering-time-points/