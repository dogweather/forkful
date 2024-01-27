---
title:                "Wykorzystanie wyrażeń regularnych"
date:                  2024-01-19
html_title:           "Arduino: Wykorzystanie wyrażeń regularnych"
simple_title:         "Wykorzystanie wyrażeń regularnych"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/cpp/using-regular-expressions.md"
---

{{< edit_this_page >}}

## What & Why? (Co i dlaczego?)
Regular expressions to wzorce służące do przeszukiwania, dopasowywania i manipulowania tekstami. Programiści wykorzystują je do efektywnego przetwarzania ciągów znaków, sprawdzania poprawności danych wejściowych czy automatyzacji zadań.

## How to: (Jak to zrobić?)
```C++
#include <iostream>
#include <regex>
using namespace std;

int main() {
    string data = "example@example.com";
    regex email_regex(R"(\b[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+\.[A-Z|a-z]{2,}\b)");

    bool is_valid = regex_match(data, email_regex);
    
    if (is_valid) {
        cout << "Valid email!" << endl;
    } else {
        cout << "Invalid email!" << endl;
    }

    return 0;
}
```
Output:
```
Valid email!
```

## Deep Dive (Głębsze spojrzenie)
Regularne wyrażenia ewoluowały od lat 50.; obecnie są niezastąpionym narzędziem w programowaniu. Alternatywami mogą być parsery czy funkcje typu `find` i `substring`, ale są one mniej elastyczne. Implementacja RegExp w C++ wykorzystuje bibliotekę `<regex>` od standardu C++11, zapewniając klasy takie jak `std::regex` i `std::smatch`.

## See Also (Zobacz także)
- [cppreference.com Regex library](https://en.cppreference.com/w/cpp/regex)
- [Regular Expressions in C++](https://www.cplusplus.com/reference/regex/)
- [Modern C++ Tutorial: C++11/14/17/20](https://changkun.de/modern-cpp/)
