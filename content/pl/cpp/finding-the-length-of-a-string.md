---
title:                "Znajdowanie długości ciągu znaków"
html_title:           "Arduino: Znajdowanie długości ciągu znaków"
simple_title:         "Znajdowanie długości ciągu znaków"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/cpp/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Co to i dlaczego?

Długość ciągu to liczba znaków, które go tworzą. Programiści potrzebują tej informacji, na przykład, do alokacji pamięci, porównywania ciągów, czy iterowania po zawartości ciągu.

## Jak to zrobić:

```C++
#include<iostream>
#include<string>

int main() {
    std::string mojString = "C++ dla Polaków";
    std::cout << "Długość ciągu: "<< mojString.length() << "\n";
    return 0;
}
```

Działa to tak:

```
Długość ciągu: 14
```

## Głębsze spojrzenie

Dawniej w C, aby znaleźć długość ciągu, używano funkcji `strlen()`, która skanowała ciąg aż do znaku końca oznaczonego przez '\0'. Teraz w C++, używa się metody `length()` klasy `std::string`, która zwraca długość ciągu w sposób bardziej efektywny i bezpieczny.

Alternatywą dla `length()` jest metoda `size()`, która zwraca dokładnie to samo. Wybór między nimi to kwestia stylistyczna.

Pod kątem implementacji, `std::string` to klasa, która przechowuje długość ciągu i zwraca ją podczas wywoływania `length()`. Nie ma więc potrzeby skanowania ciągu, co czyni operację szybką i o stałym czasie.

## Zobacz także

- Dokumentacja `std::string::length`: http://www.cplusplus.com/reference/string/string/length/
- Dokumentacja `std::string::size`: http://www.cplusplus.com/reference/string/string/size/
- Jak działa `strlen`: https://stackoverflow.com/questions/29200635/compare-strlen-and-string-length-in-c