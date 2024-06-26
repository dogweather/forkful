---
date: 2024-01-20 17:57:42.538266-07:00
description: "Jak to zrobi\u0107: Zast\u0119powanie tekstu to stary, uniwersalny trik.\
  \ Programi\u015Bci korzystaj\u0105 z tego od dekad, szczeg\xF3lnie kiedy trzeba\
  \ by\u0142o dba\u0107 o oszcz\u0119dno\u015B\u0107\u2026"
lastmod: '2024-04-05T21:53:37.123848-06:00'
model: gpt-4-1106-preview
summary: "Zast\u0119powanie tekstu to stary, uniwersalny trik."
title: Wyszukiwanie i zamiana tekstu
weight: 10
---

## Jak to zrobić:
```C++
#include <iostream>
#include <string>
#include <regex>

int main() {
    std::string tekst = "Ala ma kota, a kot ma Alę.";
    std::regex wzorzec("kot");
    std::string nowy_fragment = "pies";

    // Zastępowanie wszystkich wystąpień wzorca 'kot' na 'pies'
    std::string zmieniony_tekst = std::regex_replace(tekst, wzorzec, nowy_fragment);
    
    std::cout << zmieniony_tekst << std::endl; // Wyświetli: Ala ma psa, a pies ma Alę.
    
    return 0;
}
```

## Głębsze spojrzenie:
Zastępowanie tekstu to stary, uniwersalny trik. Programiści korzystają z tego od dekad, szczególnie kiedy trzeba było dbać o oszczędność pamięci. Historia narzędzi typu `sed` w Unix czy `Find and Replace` w edytorach tekstu pokazuje jego znaczenie. Alternatywy? Biblioteki do manipulacji stringami, jak `Boost`, czy języki z potężnymi narzędziami do pracy z tekstem, np. Perl. We współczesnym C++ korzystamy z `std::regex` dla wyrażeń regularnych i `std::string` dla manipulacji ciągami znaków.

## Zobacz również:
- [cppreference std::regex](https://en.cppreference.com/w/cpp/regex)
- [cplusplus.com std::string](http://www.cplusplus.com/reference/string/string/)

Pamiętaj, że poradniki i dokumentacje bywają najlepszym źródłem głębszej wiedzy. Ćwiczenie czyni mistrza, więc testuj różne metody i znajdź tę idealną dla swojego projektu.
