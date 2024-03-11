---
date: 2024-01-20 17:45:25.531448-07:00
description: "Wyodr\u0119bnianie podci\u0105g\xF3w to proces wycinania fragment\xF3\
  w z wi\u0119kszego ci\u0105gu znak\xF3w. Programi\u015Bci robi\u0105 to, aby manipulowa\u0107\
  \ i analizowa\u0107 tekst, weryfikowa\u0107\u2026"
lastmod: '2024-03-11T00:14:08.900318-06:00'
model: gpt-4-1106-preview
summary: "Wyodr\u0119bnianie podci\u0105g\xF3w to proces wycinania fragment\xF3w z\
  \ wi\u0119kszego ci\u0105gu znak\xF3w. Programi\u015Bci robi\u0105 to, aby manipulowa\u0107\
  \ i analizowa\u0107 tekst, weryfikowa\u0107\u2026"
title: "Wycinanie pod\u0142a\u0144cuch\xF3w"
---

{{< edit_this_page >}}

## Co i dlaczego?

Wyodrębnianie podciągów to proces wycinania fragmentów z większego ciągu znaków. Programiści robią to, aby manipulować i analizować tekst, weryfikować dane wejściowe, a także wydobywać informacje potrzebne do dalszego przetwarzania.

## Jak to zrobić:

Oto przykład w C++. Załóżmy, że chcesz wydobyć podciąg z tekst "Dzień dobry, Coders!".

```C++
#include <iostream>
#include <string>

int main() {
    std::string fullString = "Dzień dobry, Coders!";
    std::string subString = fullString.substr(12, 7); // Start at index 12, length 7

    std::cout << subString << std::endl; // Wypisze "Coders"
    
    return 0;
}
```
Wyjście:
```
Coders
```

## W pogłębieniu:

Wycinanie podciągów nie zmieniło się wiele od wprowadzenia C++ w latach 80-tych. Ale wtedy korzystano głównie z tablic znaków typu `char[]` i funkcji takich jak `strncpy()`. Od C++11, łatwiej jest używać klas `std::string` i ich metod takich jak `substr()`. 

Dla porównania, w innych językach jak Python, podciągi wydobywa się przez `slicing`, co jest bardziej elastyczne. W C++, oprócz metody `substr()`, możesz także użyć iteratorów klasy `string`, aby wydobyć podciąg na różne sposoby, co daje ci więcej kontroli nad operacją.

Implementacja `substr()` w standardowej bibliotece C++ może się różnić w zależności od kompilatora, ale zazwyczaj jest to operacja o złożoności czasowej O(n), gdzie n to długość substringu.

## Zobacz również:

- Dokumentacja C++ na cppreference.com: [std::basic_string::substr](https://en.cppreference.com/w/cpp/string/basic_string/substr)
- Przewodnik po C++ z cplusplus.com: [String - substr()](http://www.cplusplus.com/reference/string/string/substr/)
- Porównanie metody `substr()` z C++ z metodami w innych językach: [Rosetta Code - Substring](https://rosettacode.org/wiki/Substring)
