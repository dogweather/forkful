---
date: 2024-01-26 03:38:11.998793-07:00
description: "Jak to zrobi\u0107: Oto prosty spos\xF3b, aby pozby\u0107 si\u0119 cudzys\u0142\
  ow\xF3w w C++."
lastmod: '2024-03-13T22:44:35.700857-06:00'
model: gpt-4-0125-preview
summary: "Oto prosty spos\xF3b, aby pozby\u0107 si\u0119 cudzys\u0142ow\xF3w w C++."
title: "Usuwanie cudzys\u0142ow\xF3w z ci\u0105gu znak\xF3w"
weight: 9
---

## Jak to zrobić:
Oto prosty sposób, aby pozbyć się cudzysłowów w C++:

```cpp
#include <iostream>
#include <algorithm>

std::string remove_quotes(std::string input) {
    input.erase(std::remove(input.begin(), input.end(), '\"'), input.end());
    input.erase(std::remove(input.begin(), input.end(), '\''), input.end());
    return input;
}

int main() {
    std::string original = R"("Hello, 'World'!")";
    std::string no_quotes = remove_quotes(original);
    std::cout << no_quotes << std::endl;
    return 0;
}
```

Uruchom to, a otrzymasz:

```
Hello, World!
```

I proszę! Cudzysłowy zniknęły.

## Szczegółowa analiza
Cudzysłowy są utrapieniem w tekście od zarania komputeryzacji. W przeszłości widywało się programistów, którzy mozolnie przeszukiwali każdy znak, aby odfiltrować te cudzysłowy. Dzisiaj mamy `std::remove` w Standard Template Library (STL), aby wykonać tę ciężką pracę.

Alternatywy? Jasne! Można użyć wyrażeń regularnych z `std::regex`, aby celować w cudzysłowy, ale to trochę jak używanie młota kowalskiego do pęknięcia orzecha - potężne, ale może być nadmiarem dla prostych zadań. Dla tych, którzy preferują nowsze wersje C++, można pokusić się o `std::string_view` dla metod niezmieniających.

Jeśli chodzi o implementację, należy pamiętać, że `std::remove` faktycznie nie usuwa elementów z kontenera; przesuwa elementy nieusunięte do przodu i zwraca iterator za nowy koniec zakresu. Dlatego potrzebujemy metody `erase`, aby odciąć niechciany ogon.

## Zobacz też
- Referencje C++ `std::remove`: [cppreference.com](https://en.cppreference.com/w/cpp/algorithm/remove)
- Więcej o manipulacji `std::string`: [cplusplus.com](http://www.cplusplus.com/reference/string/string/)
