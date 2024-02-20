---
date: 2024-01-26 03:38:11.998793-07:00
description: "Usuwanie cudzys\u0142ow\xF3w z ci\u0105gu znak\xF3w oznacza pozbycie\
  \ si\u0119 tych irytuj\u0105cych podw\xF3jnych lub pojedynczych znak\xF3w, kt\xF3\
  re otaczaj\u0105 nasz tekst (' lub \").\u2026"
lastmod: 2024-02-19 22:04:54.846360
model: gpt-4-0125-preview
summary: "Usuwanie cudzys\u0142ow\xF3w z ci\u0105gu znak\xF3w oznacza pozbycie si\u0119\
  \ tych irytuj\u0105cych podw\xF3jnych lub pojedynczych znak\xF3w, kt\xF3re otaczaj\u0105\
  \ nasz tekst (' lub \").\u2026"
title: "Usuwanie cudzys\u0142ow\xF3w z ci\u0105gu znak\xF3w"
---

{{< edit_this_page >}}

## Co i dlaczego?
Usuwanie cudzysłowów z ciągu znaków oznacza pozbycie się tych irytujących podwójnych lub pojedynczych znaków, które otaczają nasz tekst (' lub "). Programiści często robią to, aby oczyścić dane wejściowe, przechować tekst w bazie danych lub przygotować ciągi znaków do dalszej obróbki bez bałaganu związanego z cudzysłowami.

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
