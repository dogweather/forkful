---
date: 2024-01-20 17:47:33.452863-07:00
description: "How to: (Jak to zrobi\u0107:) W C++ d\u0142ugo\u015B\u0107 \u0142a\u0144\
  cucha znak\xF3w mo\u017Cesz znale\u017A\u0107 u\u017Cywaj\u0105c funkcji `length()`\
  \ lub `size()` na obiekcie `std::string`, jak poni\u017Cej."
lastmod: '2024-03-13T22:44:35.703875-06:00'
model: gpt-4-1106-preview
summary: "W C++ d\u0142ugo\u015B\u0107 \u0142a\u0144cucha znak\xF3w mo\u017Cesz znale\u017A\
  \u0107 u\u017Cywaj\u0105c funkcji `length()` lub `size()` na obiekcie `std::string`,\
  \ jak poni\u017Cej."
title: "Znalezienie d\u0142ugo\u015Bci ci\u0105gu znak\xF3w"
weight: 7
---

## How to: (Jak to zrobić:)
W C++ długość łańcucha znaków możesz znaleźć używając funkcji `length()` lub `size()` na obiekcie `std::string`, jak poniżej:

```C++
#include <iostream>
#include <string>

int main() {
    std::string text = "Cześć, jak się masz?";
    std::cout << "Długość tekstu: " << text.length() << std::endl;
    // Lub używając size()
    std::cout << "Długość tekstu (size): " << text.size() << std::endl;

    return 0;
}
```

Sample output:
```
Długość tekstu: 22
Długość tekstu (size): 22
```

## Deep Dive (Wgłębienie się)
Historia metody znalezienia długości łańcucha znaków sięga wczesnych języków programowania, gdzie operacje na tekstach były podstawowe. W C tradycyjne łańcuchy C-style kończą się znakiem `'\0'`, więc długość można było znaleźć iterując przez łańcuch aż do napotkania tego znaku. W C++ obiekt `std::string` już zawiera informacje o długości, co upraszcza sprawę.

Jako alternatywę w C++17 wprowadzono `std::string_view`, które umożliwia operacje na tekstach bez kopiiowania danych, co może być wydajniejsze. Obiekt `std::string_view` również posiada metody `length()` i `size()`.

W żadnym z przypadków wywołania `size()` czy `length()` nie są operacjami o złożoności większej niż O(1), ponieważ długość jest trzymana wewnątrz obiektu `std::string` jako wartość.

## See Also (Zobacz także)
- [http://www.cplusplus.com/reference/string/string/length/](http://www.cplusplus.com/reference/string/string/length/) - Dokumentacja `std::string::length()`.
- [http://www.cplusplus.com/reference/string/string/size/](http://www.cplusplus.com/reference/string/string/size/) - Dokumentacja `std::string::size()`.
- [https://en.cppreference.com/w/cpp/string/basic_string_view](https://en.cppreference.com/w/cpp/string/basic_string_view) - Dokumentacja `std::string_view`.
