---
title:                "Interpolacja łańcuchów znaków"
date:                  2024-01-20T17:50:17.479268-07:00
model:                 gpt-4-1106-preview
simple_title:         "Interpolacja łańcuchów znaków"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/cpp/interpolating-a-string.md"
---

{{< edit_this_page >}}

## What & Why? (Co i Dlaczego?)
Interpolacja stringów pozwala na wstawienie wartości zmiennych bezpośrednio w ciąg tekstowy. Programiści używają jej, aby łatwiej formatować wiadomości i skuteczniej budować dynamiczne ciągi znaków.

## How to (Jak to zrobić):
Od C++20 mamy dostępne formatowanie stylu `{fmt}`. Sprawdź:

```C++
#include <iostream>
#include <format>

int main() {
    std::string name = "Marek";
    int age = 30;
    
    std::string greeting = std::format("Cześć, {}! Masz {} lat.", name, age);
    std::cout << greeting << std::endl;
    
    return 0;
}
```

Wynik:
```
Cześć, Marek! Masz 30 lat.
```

## Deep Dive (Dogłębna analiza)
Interpolacja stringów znana jest z wielu języków, takich jak Python czy JavaScript. W C++ analogiczną funkcjonalność powszechnie osiągano przez `printf` lub strumienie (np. `std::ostringstream`). Formatowanie stylu `{fmt}` (od C++20) jest inspirowane biblioteką fmt i Pythonem, oferuje typowane bezpieczeństwo i jest wygodniejsze w użyciu niż starsze metody.

Alternatywy:
- `sprintf` / `snprintf`: starsze funkcje C-style, które mogą prowadzić do błędów i wycieków pamięci.
- Strumienie I/O (`std::stringstream`): szerokie możliwości ale skomplikowane i często mniej wydajne.
- Biblioteka `fmt` przed C++20: zewnętrzna biblioteka, która wprowadziła wygodne formatowanie.

Implementacja detale `std::format` to bezpieczna i elastyczna opcja formatowania stringów, która pozwala na precyzyjne sterowanie formatem wyjściowym, włączając w to szerokość pola, precyzję i wiele innych.

## See Also (Zobacz też)
- Oficjalna dokumentacja `std::format`: https://en.cppreference.com/w/cpp/utility/format/format
- Historia i wprowadzenie do biblioteki `{fmt}`: https://fmt.dev/latest/index.html
- Strona projektu C++20: https://isocpp.org/std/status
