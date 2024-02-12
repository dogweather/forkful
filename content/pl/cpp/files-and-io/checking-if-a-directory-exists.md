---
title:                "Sprawdzanie, czy katalog istnieje"
aliases: - /pl/cpp/checking-if-a-directory-exists.md
date:                  2024-02-03T19:06:53.687805-07:00
model:                 gpt-4-0125-preview
simple_title:         "Sprawdzanie, czy katalog istnieje"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/cpp/checking-if-a-directory-exists.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
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
