---
title:                "Zamiana ciągu znaków na małe litery"
html_title:           "C++: Zamiana ciągu znaków na małe litery"
simple_title:         "Zamiana ciągu znaków na małe litery"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/cpp/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Dlaczego

Konwersja na małe litery jest powszechnie używaną operacją w programowaniu. W języku C++, zmiana wszystkich znaków w ciągu znaków na ich odpowiedniki w postaci małych liter może być niezbędna do łatwego przetwarzania i porównywania danych.

## Jak to zrobić

Aby zmienić ciąg znaków na małe litery w C++, należy użyć funkcji `tolower()` z biblioteki `<cctype>`. Przykładowy kod wyglądałby następująco:

```C++
#include <iostream>
#include <cctype>

int main() {
    std::string s = "PRZYKŁADOWY TEKST";
    for (char& c : s) {
        c = std::tolower(c); // konwersja każdej litery na małą
    }
    std::cout << s; // wyświetlenie wyniku: "przykładowy tekst"
    return 0;
}
```

Można również użyć funkcji `transform()` z biblioteki `<algorithm>` w połączeniu z funkcją `tolower()`:

```C++
#include <iostream>
#include <algorithm>
#include <cctype>

int main() {
    std::string s = "PRZYKŁADOWY TEKST";
    std::transform(s.begin(), s.end(), s.begin(),
                   [](unsigned char c){ return std::tolower(c); });
    std::cout << s; // wyświetlenie wyniku: "przykładowy tekst"
    return 0;
}
```

## Głębsze zagłębienie

Ciągi znaków w języku C++ są przechowywane jako tablice typu `char`, dlatego używając funkcji `tolower()` konwertujemy pojedyncze znaki. Dzięki temu możemy uniknąć błędów, które mogłyby się pojawić przy konwersji całego ciągu naraz, gdyż znaki nie są przechowywane w pamięci w kolejności odpowiadającej wyświetleniu ich na ekranie. Dodatkowo, funkcja `tolower()` może zostać wywołana również dla pojedynczych znaków typu `wint_t`, umożliwiając konwersję znaków spoza standardowego zakresu.

## Zobacz także

- [Referencja funkcji `tolower()` w języku C++](https://www.cplusplus.com/reference/cctype/tolower/)
- [Przykłady użycia funkcji `transform()` w języku C++](https://en.cppreference.com/w/cpp/algorithm/transform)
- [Wyjaśnienie różnicy między typami `char` i `wint_t` w języku C++](https://stackoverflow.com/questions/19336433/is-wint-t-the-same-as-char-in-c)