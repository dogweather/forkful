---
title:                "Zamiana liter na wielkie w ciągu znaków"
date:                  2024-01-19
html_title:           "Arduino: Zamiana liter na wielkie w ciągu znaków"
simple_title:         "Zamiana liter na wielkie w ciągu znaków"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/cpp/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## What & Why? (Co i dlaczego?)
Kapitalizacja stringa to proces zamieniania pierwszych liter słów na wielkie litery. Programiści używają tej techniki dla lepszego czytania tekstu przez ludzi lub ustalonych standardów formatowania danych.

## How to: (Jak to zrobić:)
```C++
#include <iostream>
#include <string>
#include <cctype>

// Funkcja kapitalizująca string
std::string capitalizeString(const std::string &str) {
    std::string capitalized = str;
    bool capitalizeNext = true;

    for (char &ch : capitalized) {
        if (capitalizeNext && std::isalpha(ch)) {
            ch = std::toupper(ch);
            capitalizeNext = false;
        } else if (!std::isalnum(ch)) {
            capitalizeNext = true;
        }
    }
    return capitalized;
}

int main() {
    std::string text = "to jest przykład tekstu.";
    std::string capitalizedText = capitalizeString(text);
    std::cout << capitalizedText << std::endl; // Wydrukuj kapitalizowany tekst
    return 0;
}
```
Sample output:
```
To Jest Przykład Tekstu.
```

## Deep Dive (Dogłębna analiza)
Capitalizing a string isn't a new problem; it's been around since computers started processing text. Historically, systems like UNIX had tools (`awk`, `sed`, etc.) that could transform text. In C++, capitalization can be done manually (as shown) or with libraries like Boost.

There's more than one way to capitalize a string. One alternative is the `transform` method with a proper function from algorithms header. You could also use a regex (though it's overkill for simple cases).

The implementation detail to notice is `std::isalpha` and `std::toupper` usage, which checks for alphabetical characters and converts to uppercase, respectively. These functions handle ASCII text. For Unicode, other solutions (like ICU library) are needed.

## See Also (Zobacz także)
- [C++ reference for std::toupper](https://en.cppreference.com/w/cpp/string/byte/toupper)
- [C++ reference for std::isalpha](https://en.cppreference.com/w/cpp/string/byte/isalpha)
- [Boost String Algorithms Library](https://www.boost.org/doc/libs/release/libs/algorithm/string/)
- [C++ reference for std::transform](https://en.cppreference.com/w/cpp/algorithm/transform)
- [International Components for Unicode (ICU)](http://site.icu-project.org/home)
