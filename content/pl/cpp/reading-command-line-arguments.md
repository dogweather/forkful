---
title:                "Odczytywanie argumentów wiersza poleceń"
html_title:           "C++: Odczytywanie argumentów wiersza poleceń"
simple_title:         "Odczytywanie argumentów wiersza poleceń"
programming_language: "C++"
category:             "C++"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/cpp/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Co i Dlaczego?

Odczytywanie argumentów z wiersza poleceń to proces odczytywania i przetwarzania wartości podanych przez użytkownika podczas wywoływania programu z linii poleceń. Programiści często korzystają z tej funkcjonalności, ponieważ umożliwia ona wygodne i elastyczne dostosowywanie zachowania programu.

## Jak?

Poniżej przedstawiamy prosty przykład kodu, który odczytuje argumenty z wiersza poleceń i wyświetla je na ekranie. W tym przypadku program przyjmuje dwa argumenty: imię i nazwisko, a następnie wypisuje przywitanie z wykorzystaniem podanych danych.

```C++
#include <iostream>
using namespace std;

int main(int argc, char* argv[]) {
    // sprawdzamy, czy przekazano wystarczającą ilość parametrów
    if (argc < 3) {
        cout << "Podaj imię i nazwisko jako argumenty!" << endl;
        return 0;
    }

    // pobieramy argumenty i wypisujemy przywitanie
    string firstName = argv[1];
    string lastName = argv[2];
    cout << "Witaj, " << firstName << " " << lastName << "!" << endl;

    return 0;
}
```

Przykładowe wywołanie programu:

```
./program Jan Kowalski
```

Wyjście:

```
Witaj, Jan Kowalski!
```

## Głębszy Zanurzenie

Odczytywanie argumentów z wiersza poleceń ma swoje korzenie w systemach Unixowych, gdzie wiele programów zostało stworzonych z myślą o pracy w trybie tekstowym. Alternatywą dla odczytywania argumentów z wiersza poleceń może być korzystanie z plików konfiguracyjnych, jednak ta metoda wymaga więcej pracy od programisty i może być mniej wygodna dla użytkownika.

Powyższy przykład wykorzystuje standardową funkcję dostępną w języku C++ - `main()`, która przyjmuje dwa argumenty: `argc` oraz `argv`. Pierwszy z nich określa liczbę argumentów przekazanych do programu, a drugi jest tablicą przechowującą te argumenty. Dzięki temu programista może dostosować zachowanie swojego programu w zależności od podanych wartości.

## Zobacz także

1. [Dokumentacja języka C++](https://docs.microsoft.com/en-us/cpp/cpp/command-line-arguments?view=msvc-160)
2. [Polecenia wiersza poleceń w systemie Windows](https://www.digitalcitizen.life/command-prompt-how-use-basic-commands/)
3. [Odczytywanie parametrów programu w języku Python](https://www.tutorialspoint.com/python/python_command_line_arguments.htm)