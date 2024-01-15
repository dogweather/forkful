---
title:                "Znajdowanie długości ciągu znaków"
html_title:           "C++: Znajdowanie długości ciągu znaków"
simple_title:         "Znajdowanie długości ciągu znaków"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/cpp/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Dlaczego

Jeśli jesteś programistą, prawdopodobnie często napotykasz na sytuacje, w których musisz pracować z ciągami znaków. Aby skutecznie manipulować nimi, ważne jest, aby znać ich długość. W tym artykule pokażę Ci, jak z łatwością znaleźć długość ciągu w języku C++.

## Jak to zrobić

Ogólnie istnieją dwa sposoby na znalezienie długości ciągu w języku C++. Pierwszym sposobem jest użycie funkcji `length()` z klasy `string` z biblioteki standardowej. Przykładowe użycie wyglądałoby następująco:

```C++
#include <iostream>
#include <string>

int main() {
  std::string my_str = "Hello World";
  int length = my_str.length();

  std::cout << "Długość ciągu: " << length << std::endl;

  return 0;
}
```

Wyjście:

`Długość ciągu: 11`

Drugi sposób to użycie funkcji `strlen()` z biblioteki `<cstring>`. Jednak ta funkcja działa tylko na tablicach znaków (`char*`), więc musimy najpierw przekonwertować nasz `string` na tablicę znaków. Przykład:

```C++
#include <iostream>
#include <string>
#include <cstring>

int main() {
  std::string my_str = "Hello World";
  char my_arr[my_str.length()];
  strcpy(my_arr, my_str.c_str());
  int length = strlen(my_arr);

  std::cout << "Długość ciągu: " << length << std::endl;

  return 0;
}
```

Wyjście:

`Długość ciągu: 11`

## Deep Dive

W języku C++, ciągi znaków są reprezentowane przez tablicę znaków (`char*`) lub obiekt klasy `string`. Funkcja `length()` zwraca liczbę elementów w danym ciągu, a funkcja `strlen()` zwraca liczbę znaków w tablicy znaków, nie licząc znaku końca null (`\0`). W obu przypadkach, długość ciągu jest równa liczbie znaków, które zostały przypisane do niego.

## Zobacz także

1. [Dokumentacja funkcji length()](https://www.cplusplus.com/reference/string/string/length/)
2. [Dokumentacja funkcji strlen()](https://www.cplusplus.com/reference/cstring/strlen/)