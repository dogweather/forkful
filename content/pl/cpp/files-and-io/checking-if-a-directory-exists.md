---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:06:53.687805-07:00
description: "Sprawdzanie, czy katalog istnieje, polega na okre\u015Bleniu obecno\u015B\
  ci katalogu pod okre\u015Blon\u0105 \u015Bcie\u017Ck\u0105 przed wykonaniem operacji\
  \ takich jak czytanie z plik\xF3w\u2026"
lastmod: '2024-03-13T22:44:35.727214-06:00'
model: gpt-4-0125-preview
summary: "Sprawdzanie, czy katalog istnieje, polega na okre\u015Bleniu obecno\u015B\
  ci katalogu pod okre\u015Blon\u0105 \u015Bcie\u017Ck\u0105 przed wykonaniem operacji\
  \ takich jak czytanie z plik\xF3w\u2026"
title: Sprawdzanie, czy katalog istnieje
---

{{< edit_this_page >}}

## Co i dlaczego?
Sprawdzanie, czy katalog istnieje, polega na określeniu obecności katalogu pod określoną ścieżką przed wykonaniem operacji takich jak czytanie z plików lub zapisywanie do nich. Programiści robią to, aby uniknąć błędów związanych z operacjami na plikach, zapewniając płynniejsze i bardziej niezawodne wykonanie zadań związanych z obsługą plików w ich aplikacjach.

## Jak to zrobić:
W nowoczesnym C++ (C++17 i nowsze) można użyć biblioteki filesystem do sprawdzenia, czy katalog istnieje. Zapewnia ona prosty i ustandaryzowany sposób na wykonanie operacji na systemie plików, w tym sprawdzenie istnienia katalogu.

```cpp
#include <iostream>
#include <filesystem>

namespace fs = std::filesystem;

int main() {
    const fs::path dirPath = "/ścieżka/do/katalogu";

    if (fs::exists(dirPath) && fs::is_directory(dirPath)) {
        std::cout << "Katalog istnieje." << std::endl;
    } else {
        std::cout << "Katalog nie istnieje." << std::endl;
    }

    return 0;
}
```
Przykładowe wyjście, jeśli katalog istnieje:
```
Katalog istnieje.
```

Przykładowe wyjście, jeśli katalog nie istnieje:
```
Katalog nie istnieje.
```

Dla projektów, które jeszcze nie korzystają z C++17, lub dla dodatkowych funkcji, biblioteka Boost Filesystem jest popularnym wyborem stron trzecich, który oferuje podobną funkcjonalność.

```cpp
#include <iostream>
#include <boost/filesystem.hpp>

namespace fs = boost::filesystem;

int main() {
    const fs::path dirPath = "/ścieżka/do/katalogu";

    if (fs::exists(dirPath) && fs::is_directory(dirPath)) {
        std::cout << "Katalog istnieje." << std::endl;
    } else {
        std::cout << "Katalog nie istnieje." << std::endl;
    }

    return 0;
}
```
Korzystając z Boost Filesystem, wyjście byłoby identyczne jak w przykładzie z filesystem C++17, w zależności od istnienia katalogu pod określoną ścieżką.
