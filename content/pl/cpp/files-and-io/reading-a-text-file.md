---
date: 2024-01-20 17:53:50.873895-07:00
description: "How to: (Jak to zrobi\u0107?) Je\u015Bli plik `przykladowy_plik.txt`\
  \ zawiera."
lastmod: '2024-04-05T21:53:37.156352-06:00'
model: gpt-4-1106-preview
summary: "(Jak to zrobi\u0107?) Je\u015Bli plik `przykladowy_plik.txt` zawiera."
title: Odczytywanie pliku tekstowego
weight: 22
---

## How to: (Jak to zrobić?)
```C++
#include <iostream>
#include <fstream>
#include <string>

int main() {
    std::ifstream infile("przykladowy_plik.txt");

    if (infile.is_open()) {
        std::string line;
        while (getline(infile, line)) {
            std::cout << line << '\n';
        }
        infile.close();
    } else {
        std::cerr << "Nie można otworzyć pliku!" << std::endl;
    }

    return 0;
}
```

Jeśli plik `przykladowy_plik.txt` zawiera:
```
Witaj, świecie!
To jest tekst z pliku.
```

Wynik na ekranie będzie:
```
Witaj, świecie!
To jest tekst z pliku.
```

## Deep Dive (Głębsze spojrzenie)
Odczytywanie plików tekstowych w C++ ma długą historię. W pierwszych wersjach języka używano funkcji z biblioteki C, takich jak `fopen`, `fgets`, czy `fclose`. Nowoczesny C++ oferuje klasy jak `ifstream` do eleganckiego i efektywnego odczytu plików.

Alternatywy? Możemy używać bibliotek takich jak Boost, które oferują rozszerzone możliwości pracy z plikami. Jest też `std::filesystem` w C++17 pozwalający na proste zarządzanie plikami i katalogami.

Ważne szczegóły? Pamiętaj o kontrolowaniu otwarcia pliku (`if (infile.is_open())`) i zamykaj go po zakończeniu (`infile.close()`). Obsługa błędów jest kluczowa, by uniknąć awarii programu.

## See Also (Zobacz również)
- Dokumentacja C++ `ifstream`: http://www.cplusplus.com/reference/fstream/ifstream/
- Dokumentacja C++ `std::getline`: http://www.cplusplus.com/reference/string/string/getline/
- Poradnik C++ File I/O: https://cplusplus.com/doc/tutorial/files/ 
- Tutorial Boost Filesystem: https://www.boost.org/doc/libs/release/libs/filesystem/doc/index.htm
- Tutorial `std::filesystem` w C++17: https://en.cppreference.com/w/cpp/filesystem
