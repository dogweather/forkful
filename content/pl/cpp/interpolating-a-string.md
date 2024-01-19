---
title:                "Interpolacja ciągu znaków"
html_title:           "C++: Interpolacja ciągu znaków"
simple_title:         "Interpolacja ciągu znaków"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/cpp/interpolating-a-string.md"
---

{{< edit_this_page >}}

## Co i dlaczego?
Interpolacją napisów nazywamy proces wprowadzania zmiennych do napisów. Programiści robią to, aby dynamicznie manipulować danymi tekstowymi, zmieniając ich wartość na podstawie określonych zmiennych.

## Jak to zrobić:
Możemy to zrobić, używając funkcji `fmt::format` z biblioteki `fmtlib`. Oto przykład:

```C++
#include <fmt/core.h>

int main() {
  int age = 25;
  std::string name = "John";
  std::string s = fmt::format("Moje imię to {} i mam {} lat", name, age);
  std::cout << s << std::endl;
  return 0;
}
```
Wyjście z powyższego kodu będzie wyglądać tak:
``` 
Moje imię to John i mam 25 lat
```

## W głąb tematu
Interpolacja napisów pojawiła się początkowo w językach takich jak Perl i Python, które próbowały uprościć manipulację napisami. W C++ najpierw używaliśmy operacji konkatenacji kanonicznej, tipszącej, trunkacji, dopóki `fmtlib` nie wprowadził łatwiejszego sposobu na interpolację napisów w C++20.

Alternatywą dla `fmtlib` jest używanie klasy `std::stringstream` z biblioteki STL, jednak jest ona mniej wydajna i mniej przyjazna dla użytkownika.

Implementacja `fmt::format` korzysta z metaprogramowania szablonów, co pozwala na generację optymalnego kodu dla różnych typów argumentów. Jest to elastyczny i wydajny sposób na formatowanie napisów.

## Zobacz również
* Biblioteka [fmtlib](https://fmt.dev/latest/index.html)
* Interpolacja napisów w [Pythonie](https://docs.python.org/3/tutorial/inputoutput.html)
* Alternatywne techniki formatowania napisów w [C++](https://en.cppreference.com/w/cpp/io/manip)
* [Metaprogramowanie szablonów](https://en.cppreference.com/w/cpp/language/templates)