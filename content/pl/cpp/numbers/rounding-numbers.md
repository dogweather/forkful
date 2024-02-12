---
title:                "Zaokrąglanie liczb"
aliases: - /pl/cpp/rounding-numbers.md
date:                  2024-01-26T03:44:01.042035-07:00
model:                 gpt-4-0125-preview
simple_title:         "Zaokrąglanie liczb"

tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/cpp/rounding-numbers.md"
---

{{< edit_this_page >}}

## Co i dlaczego?
Zaokrąglanie liczb oznacza dostosowanie wartości do najbliższej liczby całkowitej lub określonej precyzji. Programiści robią to, aby upraszczać, dostosowywać do realiów świata rzeczywistego lub poprawiać wydajność poprzez pozbywanie się nadmiernej precyzji.

## Jak to zrobić:
C++ oferuje kilka sposobów na zaokrąglenie liczb, takich jak `floor()`, `ceil()`, i `round()`:

```C++
#include <iostream>
#include <cmath> // dla funkcji zaokrąglających

int main() {
    double num = 3.14;

    std::cout << "floor: " << std::floor(num) << "\n"; // Wyjście: floor: 3
    std::cout << "ceil: " << std::ceil(num) << "\n";   // Wyjście: ceil: 4
    std::cout << "round: " << std::round(num) << "\n"; // Wyjście: round: 3

    // Dla stałej precyzji, jak zaokrąglenie do dwóch miejsc po przecinku:
    double precise_num = 3.146;
    double multiplier = 100.0;
    double rounded = std::round(precise_num * multiplier) / multiplier;

    std::cout << "zaokrąglone do dwóch miejsc po przecinku: " << rounded << "\n"; // Wyjście: zaokrąglone do dwóch miejsc po przecinku: 3.15

    return 0;
}
```

## Szczegółowa analiza
Przed C++11 zaokrąglanie opierało się na ręcznych technikach lub niestandardowych bibliotekach. Dzisiaj `<cmath>` oferuje solidne metody. `floor()` zaokrągla w dół, `ceil()` zaokrągla w górę, podczas gdy `round()` kieruje do najbliższej liczby całkowitej, nawet obsługując przypadki równości (0.5) poprzez zaokrąglanie do parzystej liczby.

Zrozumienie zachowania tych funkcji jest kluczowe; na przykład liczby ujemne mogą sprawić problem (`std::round(-2.5)` daje `-2.0`).

Alternatywy? Rzutowanie na typ int po dodaniu 0.5 dla liczb dodatnich było klasycznym trikiem, ale zawodzi w przypadku liczb ujemnych i nie jest niezależne od typu. Biblioteki takie jak Boost mogą oferować bardziej subtelne podejścia, a rozszerzenia języka lub wewnętrzne funkcje kompilatora mogą być zoptymalizowane pod kątem konkretnego sprzętu.

## Zobacz również
- Referencje C++ dla `<cmath>`: https://en.cppreference.com/w/cpp/header/cmath
- Standard IEEE dla arytmetyki zmiennoprzecinkowej (IEEE 754): https://ieeexplore.ieee.org/document/4610935
- Biblioteka konwersji numerycznej Boost: https://www.boost.org/doc/libs/release/libs/numeric/conversion/
