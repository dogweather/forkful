---
title:    "C++: Odczytywanie argumentów wiersza poleceń"
keywords: ["C++"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/cpp/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Dlaczego

Czy kiedykolwiek zastanawiałeś się, jak programy wiersza poleceń pobierają informacje od użytkownika? Jeśli tak, to dobrze trafiłeś! W tym blogu dowiesz się, dlaczego jest ważne umieć czytać argumenty wiersza poleceń w języku C++.

## Jak To Zrobić

W języku C++, istnieje prosty sposób na odczytywanie argumentów wprowadzonych przez użytkownika podczas uruchamiania programu. Wystarczy użyć funkcji `int main(int argc, char* argv[])`. Jest to funkcja, która pobiera dwa argumenty - `argc`, który przechowuje liczbę argumentów oraz `argv[]`, który przechowuje właściwe wartości argumentów w postaci tablicy.

Poniżej znajduje się przykładowy kod, który demonstruje to w praktyce:

```C++
#include <iostream>

using namespace std;

int main(int argc, char* argv[]) {

    // Wypisanie wszystkich argumentów podanych przez użytkownika
    for (int i = 0; i < argc; i++) {
        cout << "Argument nr " << i << ": " << argv[i] << endl;
    }

    return 0;
}
```

**Wejście:**

```sh
./program argument1 argument2 argument3
```

**Wyjście:**

```sh
Argument nr 0: ./program
Argument nr 1: argument1
Argument nr 2: argument2
Argument nr 3: argument3
```

## Wnikliwa Analiza

Jeśli zastanawiasz się, jak to działa pod spodem, to właśnie poniżej znajdziesz odpowiedź.

Zmienna `argc` przechowuje liczbę argumentów wprowadzonych przez użytkownika, w tym również nazwę samego programu. Jest to zawsze co najmniej jeden argument, ponieważ program musi być uruchomiony przy użyciu jego nazwy.

Z kolei zmienna `argv[]` jest tablicą przechowującą wszystkie argumenty wprowadzone przez użytkownika. Indeksowanie tablicy zaczyna się od 0, więc pierwszy argument znajduje się pod indeksem 0, drugi pod indeksem 1, itd.

**ProTip:** Jeśli chcesz pobrać wartości liczbowe z argumentów, możesz użyć funkcji `atoi()` lub `atof()`.

## Zobacz również

- [Dokumentacja C++ - Argumenty Programu](https://en.cppreference.com/w/cpp/language/main_function)
- [C++ How to Program by Paul J. Deitel and Harvey Deitel](https://learning.oreilly.com/library/view/c-how-to/9780132323086/)