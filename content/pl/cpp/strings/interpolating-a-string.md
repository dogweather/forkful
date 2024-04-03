---
date: 2024-01-20 17:50:17.479268-07:00
description: "Interpolacja string\xF3w pozwala na wstawienie warto\u015Bci zmiennych\
  \ bezpo\u015Brednio w ci\u0105g tekstowy. Programi\u015Bci u\u017Cywaj\u0105 jej,\
  \ aby \u0142atwiej formatowa\u0107 wiadomo\u015Bci i\u2026"
lastmod: '2024-03-13T22:44:35.698959-06:00'
model: gpt-4-1106-preview
summary: "Interpolacja string\xF3w pozwala na wstawienie warto\u015Bci zmiennych bezpo\u015B\
  rednio w ci\u0105g tekstowy."
title: "Interpolacja \u0142a\u0144cuch\xF3w znak\xF3w"
weight: 8
---

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
