---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:05:12.375608-07:00
description: "Kapitalizacja \u0142a\u0144cucha polega na zamianie pierwszego znaku\
  \ ka\u017Cdego s\u0142owa w ci\u0105gu na wielk\u0105 liter\u0119, je\u015Bli jest\
  \ ona pisana ma\u0142\u0105 liter\u0105, zachowuj\u0105c przy tym\u2026"
lastmod: '2024-02-25T18:49:34.067862-07:00'
model: gpt-4-0125-preview
summary: "Kapitalizacja \u0142a\u0144cucha polega na zamianie pierwszego znaku ka\u017C\
  dego s\u0142owa w ci\u0105gu na wielk\u0105 liter\u0119, je\u015Bli jest ona pisana\
  \ ma\u0142\u0105 liter\u0105, zachowuj\u0105c przy tym\u2026"
title: "Zamiana liter na wielkie w \u0142a\u0144cuchu znak\xF3w"
---

{{< edit_this_page >}}

## Co i dlaczego?
Kapitalizacja łańcucha polega na zamianie pierwszego znaku każdego słowa w ciągu na wielką literę, jeśli jest ona pisana małą literą, zachowując przy tym niezmienione pozostałe znaki. Programiści często wykonują to zadanie w celu formatowania wyjść, wprowadzeń użytkownika lub przetwarzania danych, aby zapewnić spójność w sposobie prezentacji lub przetwarzania tekstu, szczególnie w interfejsach użytkownika czy zadaniach normalizacji danych.

## Jak to zrobić:
W C++ można kapitalizować łańcuch za pomocą biblioteki standardowej, bez potrzeby korzystania z bibliotek stron trzecich. Jednak dla bardziej skomplikowanych lub specyficznych zachowań kapitalizacji, biblioteki takie jak Boost mogą być bardzo pomocne. Poniżej znajdują się przykłady ilustrujące oba podejścia.

### Korzystając z biblioteki standardowej C++:

```cpp
#include <iostream>
#include <cctype> // dla std::tolower i std::toupper
#include <string>

std::string capitalizeString(const std::string& input) {
    std::string result;
    bool capitalizeNext = true;

    for (char ch : input) {
        if (std::isspace(ch)) {
            capitalizeNext = true;
        } else if (capitalizeNext) {
            ch = std::toupper(ch);
            capitalizeNext = false;
        }
        result += ch;
    }

    return result;
}

int main() {
    std::string text = "hello world from c++";
    std::string capitalizedText = capitalizeString(text);
    std::cout << capitalizedText << std::endl; // Wyjście: "Hello World From C++"
}
```

### Korzystając z biblioteki Boost:

Dla bardziej zaawansowanej manipulacji łańcuchami, w tym kapitalizacji z uwzględnieniem ustawień regionalnych, warto skorzystać z biblioteki Boost String Algo.

Najpierw upewnij się, że masz zainstalowaną i skonfigurowaną bibliotekę Boost w swoim projekcie. Następnie możesz dołączyć niezbędne nagłówki i używać jej funkcji, jak pokazano poniżej.

```cpp
#include <boost/algorithm/string.hpp>
#include <iostream>
#include <string>

int main() {
    std::string text = "hello world from c++";
    std::string capitalizedText = text;

    // kapitalizuj pierwszą literę każdego słowa
    boost::algorithm::to_lower(capitalizedText); // zapewnienie, że łańcuch jest w małych literach
    capitalizedText[0] = std::toupper(capitalizedText[0]); // kapitalizuj pierwszy znak

    for (std::size_t i = 1; i < capitalizedText.length(); ++i) {
        if (isspace(capitalizedText[i - 1])) { // kapitalizuj po spacji
            capitalizedText[i] = std::toupper(capitalizedText[i]);
        }
    }

    std::cout << capitalizedText << std::endl; // Wyjście: "Hello World From C++"
}
```

W tym przypadku Boost upraszcza niektóre zadania manipulacji łańcuchami, ale nadal wymaga indywidualnego podejścia do prawdziwej kapitalizacji, ponieważ głównie oferuje narzędzia do transformacji i konwersji wielkości liter.
