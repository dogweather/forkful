---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:27:23.974570-07:00
description: "Zapis do pliku tekstowego w C++ polega na tworzeniu lub otwieraniu pliku,\
  \ a nast\u0119pnie zapisywaniu w nim danych, co stanowi podstawowe zadanie dla\u2026"
lastmod: '2024-03-13T22:44:35.731127-06:00'
model: gpt-4-0125-preview
summary: "Zapis do pliku tekstowego w C++ polega na tworzeniu lub otwieraniu pliku,\
  \ a nast\u0119pnie zapisywaniu w nim danych, co stanowi podstawowe zadanie dla aplikacji\
  \ potrzebuj\u0105cych zachowa\u0107 dane, takie jak logi, tre\u015Bci generowane\
  \ przez u\u017Cytkownik\xF3w lub ustawienia konfiguracyjne."
title: Pisanie pliku tekstowego
weight: 24
---

## Jak to zrobić:
C++ oferuje kilka sposobów na zapis do pliku tekstowego, ale jedną z najprostszych metod jest użycie biblioteki `<fstream>`, która zapewnia klasę `ofstream` (strumień wyjściowy do pliku) przeznaczoną do operacji zapisu do pliku.

### Przykład użycia `<fstream>`:
```cpp
#include <fstream>
#include <iostream>

int main() {
    std::ofstream file("example.txt");
    if (file.is_open()) {
        file << "Witaj, świecie!\n";
        file << "Zapisywanie do pliku w C++ jest proste.";
        file.close();
    } else {
        std::cerr << "Nie udało się otworzyć pliku\n";
    }
    return 0;
}
```

**Przykładowa zawartość 'example.txt':**
```
Witaj, świecie!
Zapisywanie do pliku w C++ jest proste.
```

W przypadku bardziej złożonych danych lub gdy potrzebna jest większa kontrola nad procesem zapisu, programiści mogą zwrócić się ku bibliotekom stron trzecich, takim jak Boost Filesystem.

### Przykład użycia Boost Filesystem:
Aby używać Boost do operacji na plikach, najpierw musisz zainstalować biblioteki Boost. Poniższy przykład demonstruje tworzenie i zapisywanie do pliku przy użyciu `boost::filesystem` i `boost::iostreams`.

```cpp
#include <boost/filesystem.hpp>
#include <boost/iostreams/device/file.hpp>
#include <boost/iostreams/stream.hpp>
#include <iostream>

namespace io = boost::iostreams;
namespace fs = boost::filesystem;

int main() {
    fs::path filePath("boost_example.txt");
    io::stream_buffer<io::file_sink> buf(filePath.string());
    std::ostream out(&buf);
    out << "Boost ułatwia operacje na plikach.\n";
    out << "To jest linia napisana z Boost.";
    
    return 0;
}
```

**Przykładowa zawartość 'boost_example.txt':**
```
Boost ułatwia operacje na plikach.
To jest linia napisana z Boost.
```

Wybór pomiędzy surowym C++ a biblioteką stron trzecich taką jak Boost może zależeć od konkretnych wymagań twojego projektu oraz od tego, ile kontroli lub elastyczności potrzebujesz nad operacjami we/wy na plikach.
