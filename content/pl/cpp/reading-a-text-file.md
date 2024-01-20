---
title:                "Czytanie pliku tekstowego"
html_title:           "C: Czytanie pliku tekstowego"
simple_title:         "Czytanie pliku tekstowego"
programming_language: "C++"
category:             "C++"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/cpp/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Co i Dlaczego?

Czytanie pliku tekstowego to proces, w którym program komputerowy odczytuje dane z pliku zapisanego w formacie tekstowym. Programiści robią to, aby obsłużyć informacje zapisane w plikach, takie jak konfiguracje, dane wejściowe lub zapisy dzienników.

## Jak to zrobić:

Zacznijmy od podstaw. Oto proste czytanie pliku w języku C++.

```c++
#include <iostream>
#include <fstream>
#include <string>

int main() {
   std::ifstream plik("plik.txt");
   std::string linia;
   while (getline(plik, linia)) {
      std::cout << linia << '\n';
   }
   return 0;
}
```
Wyjście programu to zawartość pliku `plik.txt`, wydrukowana linia po linii.

## Deep Dive

Czytanie plików tekstowych to podstawa operacji I/O w wielu językach programowania, a C++ nie jest wyjątkiem. Pioneerskie języki, takie jak FORTRAN i COBOL, już miały wsparcie na poziomie języka dla operacji I/O na plikach.

Alternatywą dla metody pokazanej powyżej jest użycie bibliotek zewnętrznych, które mogą oferować funkcje odczytu plików zaawansowane lub lepiej dostosowane do Twojego przypadku użytkowania.

Pod kątem implementacji, kiedy otwierasz plik w C++, tworzony jest strumień, który jest abstrakcją umożliwiającą operacje wejścia/wyjścia. Możliwe jest również otwarcie pliku w trybie binarnym, co jest bardziej wydajne, ale nie jest to odpowiednie dla ludzko-czytelnych plików tekstowych.

## Zobacz też

1. [I/O Stream Library](https://en.cppreference.com/w/cpp/io): Więcej informacji o bibliotece strumieniowej C++.
2. [File I/O in C++](http://www.cplusplus.com/doc/tutorial/files/): Tutorial do pierwszych kroków w I/O plików w C++.
3. [Boost Filesystem Library](https://www.boost.org/doc/libs/1_75_0/libs/filesystem/doc/index.htm): Zaawansowana biblioteka do operacji na plikach.
4. [C++ I/O Streams and File I/O](https://www.programiz.com/cpp-programming/file-io): Więcej przykładów i szczegółów na temat obsługi plików w C++.