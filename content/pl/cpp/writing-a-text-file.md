---
title:                "Zapisywanie pliku tekstowego"
html_title:           "Arduino: Zapisywanie pliku tekstowego"
simple_title:         "Zapisywanie pliku tekstowego"
programming_language: "C++"
category:             "C++"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/cpp/writing-a-text-file.md"
---

{{< edit_this_page >}}

## What & Why?
("## Co i dlaczego?")
Pisanie do pliku tekstowego to zapisywanie danych w czytelnej formie. Programiści robią to, żeby przechowywać wyniki, konfigurować programy lub logować działania systemu.

## How to:
("## Jak to zrobić:")
```C++
#include <fstream>
#include <iostream>

int main() {
    std::ofstream plik("przyklad.txt");
    if (plik.is_open()) {
        plik << "Cześć, to jest tekst w pliku!\n";
        plik.close();
    } else {
        std::cout << "Nie udało się otworzyć pliku!" << std::endl;
    }
    return 0;
}
```
Wyjście (zawartość `przyklad.txt`):
```
Cześć, to jest tekst w pliku!
```

## Deep Dive
("## W głąb tematu")
Sposoby zapisywania danych do plików tekstowych areną się z czasem. O ile kiedyś używano funkcji z języka C, jak `fprintf`, o tyle C++ wprowadził strumienie, które są bezpieczniejsze i łatwiejsze w użyciu. Poza `ofstream` (output file stream) istnieją inne typy, jak `fstream` (do odczytu i zapisu) czy `stringstream` (do operowania na stringach jako strumieniach). Nie zapominajmy o trybach – `ios::app` (do dopisywania), `ios::trunc` (do czyszczenia pliku przy otwieraniu). Trzeba pamiętać, że operacje na plikach mogą zakończyć się niepowodzeniem, dlatego `is_open()` jest kluczowe.

## See Also
("## Zobacz również")
- Dokumentacja C++ `fstream`: https://en.cppreference.com/w/cpp/io/basic_fstream
- Przewodnik po strumieniach C++: https://www.cplusplus.com/reference/iostream/
- Artykuł o różnicach między C a C++ w kontekście obsługi plików: https://www.geeksforgeeks.org/file-handling-c-classes-vs-c-functions/