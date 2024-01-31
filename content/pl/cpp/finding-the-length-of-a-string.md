---
title:                "Znalezienie długości ciągu znaków"
date:                  2024-01-20T17:47:33.452863-07:00
model:                 gpt-4-1106-preview
simple_title:         "Znalezienie długości ciągu znaków"

category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/cpp/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## What & Why? (Co i Dlaczego?)
Znalezienie długości łańcucha znaków to sposób na określenie, ile znaków zawiera dany tekst. Programiści robią to, aby manipulować tekstami, walidować dane wejściowe lub po prostu określić potrzebne zasoby.

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
