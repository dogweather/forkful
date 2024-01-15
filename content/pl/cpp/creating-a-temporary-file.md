---
title:                "Tworzenie pliku tymczasowego"
html_title:           "C++: Tworzenie pliku tymczasowego"
simple_title:         "Tworzenie pliku tymczasowego"
programming_language: "C++"
category:             "C++"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/cpp/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Dlaczego
Tworzenie plików tymczasowych jest częstym działaniem w wielu programach C++, zwłaszcza w przypadku operacji na danych tymczasowych lub debugowania kodu. Są one wygodnym narzędziem do tworzenia i przechowywania tymczasowych informacji w pamięci, co może ułatwić proces programowania.

## Jak to zrobić
```C++
#include <iostream>
#include <fstream>

using namespace std;

int main() {
  // Tworzenie pliku tymczasowego
  ofstream temp_file("temp.txt");

  // Sprawdzenie czy plik został utworzony
  if (temp_file.is_open()) {
    // Zapisanie danych do pliku
    temp_file << "To jest przykładowy tekst w pliku tymczasowym.";

    // Zamknięcie pliku
    temp_file.close();

    // Wyświetlenie informacji o sukcesie
    cout << "Plik tymczasowy został utworzony i zapisany." << endl;
  }
  else {
    // Wyświetlenie informacji o błędzie
    cout << "Nie udało się utworzyć pliku tymczasowego." << endl;
  }

  return 0;
}
```

Przykładowy wynik:

```
Plik tymczasowy został utworzony i zapisany.
```

## Deep Dive
Tworzenie pliku tymczasowego w języku C++ wymaga użycia strumienia plików (fstream). Przy użyciu funkcji `open()`, można utworzyć nowy plik lub otworzyć istniejący. Natomiast za pomocą funkcji `close()` plik zostaje zamknięty i zmiany są zapisane.

Warto również zwrócić uwagę, że pliki tymczasowe są automatycznie usuwane po zakończeniu działania programu. Można również ręcznie usunąć plik za pomocą funkcji `remove()`.

## Zobacz także
- [Dokumentacja języka C++ na stronie cppreference.com](https://en.cppreference.com/w/cpp)
- [Tworzenie i zarządzanie plikami w C++](https://www.geeksforgeeks.org/file-handling-c-classes/)
- [Tworzenie tymczasowych plików w C++](https://www.techiedelight.com/create-temporary-file-directory-cpp/)