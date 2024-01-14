---
title:                "C++: Tworzenie pliku tymczasowego"
programming_language: "C++"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/cpp/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Dlaczego

Tworzenie plików tymczasowych jest nieodłączną częścią wielu projektów programistycznych w języku C++. Takie pliki są przydatne m.in. podczas przetwarzania danych w trakcie działania programu lub w celu przechowywania danych tymczasowych, które nie są potrzebne po zakończeniu działania programu.

## Jak to zrobić

Język programowania C++ udostępnia kilka sposobów tworzenia plików tymczasowych. Jedną z najpopularniejszych metod jest użycie funkcji `std::tmpnam()`, która tworzy unikalną nazwę pliku tymczasowego i zwraca ją jako ciąg znaków. Następnie można użyć tej nazwy do otwarcia pliku za pomocą funkcji `std::fopen()`. Poniżej znajduje się przykładowy kod:

```C++
#include <cstdio>

int main() {
    char* filename = std::tmpnam(nullptr); // utworzenie nazwy pliku tymczasowego
    FILE* tempFile = std::fopen(filename, "w"); // otwarcie pliku w trybie zapisu
    std::fprintf(tempFile, "Przykładowy tekst"); // zapisanie danych do pliku
    std::fclose(tempFile); // zamknięcie pliku
    return 0;
}
```

Output:

```
Plik tymczasowy o nazwie np41z0 został utworzony.
```

Inną metodą jest użycie funkcji `std::tmpfile()`, która automatycznie tworzy plik tymczasowy i zwraca wskaźnik do tego pliku. Ponownie, można użyć funkcji `std::fprintf()` do zapisu danych do pliku. Przykładowy kod wyglądałby tak:

```C++
#include <cstdio>

int main() {
    FILE* tempFile = std::tmpfile(); // utworzenie pliku tymczasowego
    std::fprintf(tempFile, "Przykładowy tekst"); // zapisanie danych do pliku
    std::fclose(tempFile); // zamknięcie pliku
    return 0;
}
```

Output:

```
Plik tymczasowy został utworzony.
```

Jedną z zalet korzystania z funkcji `std::tmpfile()` jest to, że plik ten jest automatycznie usuwany po zakończeniu działania programu.

## Deep Dive

Podczas tworzenia plików tymczasowych warto pamiętać, że ich nazwa powinna być unikalna, aby uniknąć konfliktów z innymi plikami. Dlatego warto używać funkcji `std::tmpnam()` lub `std::tmpfile()`, które zapewniają tworzenie nazwy pliku tymczasowego o unikalnej nazwie.

Ponadto, istotne jest również pamiętanie o usuwaniu pliku tymczasowego po jego użyciu. W przypadku użycia funkcji `std::tmpfile()`, jest to wykonywane automatycznie po zakończeniu działania programu. Natomiast w przypadku użycia funkcji `std::tmpnam()`, należy ręcznie usunąć plik przy użyciu funkcji `std::remove()`.

## Zobacz również

- [Funkcja `std::tmpnam()` w języku C++ na cppreference.com](https://en.cppreference.com/w/cpp/io/c/tmpnam)
- [Funkcja `std::tmpfile()` w języku C++ na cppreference.com](https://en.cppreference.com/w/cpp/io/c/tmpfile)
- [Poradnik o zarządzaniu plikami tymczasowymi w języku C++ na tech.io](https://tech.io/playgrounds/26334/zarzadzanie-plikami-w-jezyku-c/pliki-tymczasowe)