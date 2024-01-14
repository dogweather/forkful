---
title:    "C++: Tworzenie pliku tekstowego"
keywords: ["C++"]
---

{{< edit_this_page >}}

## Dlaczego

Pisanie plików tekstowych jest nieodłączną częścią programowania w języku C++. Umożliwia to przechowywanie danych w plikach dla późniejszego użycia lub wykorzystania przez inne programy.

## Jak

Aby napisać plik tekstowy w C++, musimy najpierw otworzyć plik w trybie do zapisu przy użyciu funkcji `open()` z biblioteki `fstream`. Następnie w celu zapisania danych do pliku, należy użyć funkcji `<<` lub `write()` wraz z wybranymi danymi wewnątrz strumienia plikowego. Na końcu nie zapomnijmy zamknąć pliku za pomocą funkcji `close()`.

```C++
#include <iostream>
#include <fstream>

int main()
{
    // otwarcie pliku do zapisu
    std::ofstream plik;
    plik.open("moj_plik.txt");

    // zapisanie danych do pliku
    plik << "To jest przykładowy tekst.\n";
    int liczba = 10;
    plik << "Liczba: " << liczba << "\n";
    double dlugosc = 3.14;
    plik << "Długość: " << dlugosc << "\n";

    // zamknięcie pliku
    plik.close();

    return 0;
}
```

Po uruchomieniu powyższego kodu, powinien zostać utworzony plik o nazwie "moj_plik.txt" zawierający tekst i liczby.

## Głębsze zagadnienia

Przy zapisywaniu plików tekstowych należy pamiętać o używaniu odpowiednich typów danych, tak aby dane były zgodne ze specyfikacją danego pliku. Inne ważne aspekty to obsługa błędów i wyjątków, a także praca z plikami o większych rozmiarach danych.

## Zobacz też

- [Oficjalna dokumentacja języka C++](https://en.cppreference.com/w/)
- [Podstawowe operacje na plikach w C++](https://www.tutorialspoint.com/cplusplus/cpp_files_streams.htm)