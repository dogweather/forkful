---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:06:53.687805-07:00
description: "Jak to zrobi\u0107: W nowoczesnym C++ (C++17 i nowsze) mo\u017Cna u\u017C\
  y\u0107 biblioteki filesystem do sprawdzenia, czy katalog istnieje. Zapewnia ona\
  \ prosty i\u2026"
lastmod: '2024-03-13T22:44:35.727214-06:00'
model: gpt-4-0125-preview
summary: "W nowoczesnym C++ (C++17 i nowsze) mo\u017Cna u\u017Cy\u0107 biblioteki\
  \ filesystem do sprawdzenia, czy katalog istnieje."
title: Sprawdzanie, czy katalog istnieje
weight: 20
---

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
