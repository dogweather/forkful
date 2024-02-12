---
title:                "Wyszukiwanie i zamiana tekstu"
aliases:
- /pl/cpp/searching-and-replacing-text/
date:                  2024-01-20T17:57:42.538266-07:00
model:                 gpt-4-1106-preview
simple_title:         "Wyszukiwanie i zamiana tekstu"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/cpp/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Co i dlaczego?
Zastępowanie tekstu to podmiana jednego ciągu znaków innym. Programiści robią to, by szybko modyfikować dane czy kod - poprawiają błędy, aktualizują informacje, dostosowują do nowych wymagań.

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
