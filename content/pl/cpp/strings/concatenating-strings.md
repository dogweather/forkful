---
date: 2024-01-20 17:34:12.479530-07:00
description: "How to: (Jak to zrobi\u0107?) C++ oferuje kilka sposob\xF3w na \u0142\
  \u0105czenie \u0142a\u0144cuch\xF3w znak\xF3w. Sp\xF3jrzmy na przyk\u0142ady."
lastmod: '2024-04-05T21:53:37.131076-06:00'
model: gpt-4-1106-preview
summary: "(Jak to zrobi\u0107?) C++ oferuje kilka sposob\xF3w na \u0142\u0105czenie\
  \ \u0142a\u0144cuch\xF3w znak\xF3w."
title: "\u0141\u0105czenie \u0142a\u0144cuch\xF3w znak\xF3w"
weight: 3
---

## How to: (Jak to zrobić?)
C++ oferuje kilka sposobów na łączenie łańcuchów znaków. Spójrzmy na przykłady:

```C++
#include <iostream>
#include <string>

int main() {
    // Używając operatora +
    std::string hello = "Hello, ";
    std::string world = "World!";
    std::string greeting = hello + world;
    std::cout << greeting << std::endl; // Output: Hello, World!

    // Używając metody append()
    std::string name = "Jan";
    std::string welcome = "Witaj, ";
    welcome.append(name);
    std::cout << welcome << std::endl; // Output: Witaj, Jan

    // Używając stringstream
    #include <sstream>
    std::stringstream ss;
    ss << hello << world;
    std::cout << ss.str() << std::endl; // Output: Hello, World!
    
    return 0;
}
```

## Deep Dive (Głebokie zanurzenie)
Łączenie łańcuchów znaków, czyli konkatenacja, jest od dawna w C++. W wersjach przed C++11, często używano `char` arrays i funkcji `strcat`, ale były uciążliwe i mniej bezpieczne. Od C++11 mamy `std::string`, co ułatwia życie.

Alternatywnie, używamy `std::stringstream` do skomplikowanego składania tekstu, ale to może być wolniejsze. Pamiętajcie też o `operator+=`, gdybyście chcieli do istniejącego łańcucha dodać coś na końcu.

Z pod kątem implementacji, operator `+` tworzy nowy łańcuch, podczas gdy `append()` i `operator+=` modyfikują istniejący, co jest zwykle wydajniejsze.

## See Also (Zobacz również)
- [cppreference.com](https://en.cppreference.com/w/cpp/string/basic_string)
- [cplusplus.com](http://www.cplusplus.com/reference/string/string/)
- [Stringstream reference](https://en.cppreference.com/w/cpp/io/basic_stringstream)
