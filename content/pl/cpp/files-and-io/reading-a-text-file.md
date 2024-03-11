---
date: 2024-01-20 17:53:50.873895-07:00
description: "Odczytywanie pliku tekstowego to proces wydobycia informacji zapisanych\
  \ w pliku na dysku. Programi\u015Bci robi\u0105 to, by umo\u017Cliwi\u0107 aplikacjom\
  \ korzystanie z\u2026"
lastmod: '2024-03-11T00:14:08.932207-06:00'
model: gpt-4-1106-preview
summary: "Odczytywanie pliku tekstowego to proces wydobycia informacji zapisanych\
  \ w pliku na dysku. Programi\u015Bci robi\u0105 to, by umo\u017Cliwi\u0107 aplikacjom\
  \ korzystanie z\u2026"
title: Odczytywanie pliku tekstowego
---

{{< edit_this_page >}}

## What & Why? (Co i dlaczego?)
Odczytywanie pliku tekstowego to proces wydobycia informacji zapisanych w pliku na dysku. Programiści robią to, by umożliwić aplikacjom korzystanie z danych, takich jak ustawienia, dane wejściowe użytkownika czy zawartość do przetworzenia.

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
