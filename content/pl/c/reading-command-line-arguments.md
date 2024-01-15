---
title:                "Odczytywanie argumentów wiersza poleceń"
html_title:           "C: Odczytywanie argumentów wiersza poleceń"
simple_title:         "Odczytywanie argumentów wiersza poleceń"
programming_language: "C"
category:             "C"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/c/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Dlaczego

C jest językiem programowania, który pozwala na obsługę argumentów wiersza poleceń. W tym artykule dowiesz się, dlaczego warto poznać tę umiejętność.

## Jak to zrobić

Aby czytać argumenty wiersza poleceń w C, użyj funkcji main z argumentami argc (licznik argumentów) i argv (tablica argumentów). Przykład kodu wygląda następująco:

```C
#include <stdio.h>

int main(int argc, char *argv[]) {
    for (int i = 0; i < argc; i++) {
        printf("Argument %d to %s.\n", i, argv[i]);
    }
    return 0;
}
```

Po uruchomieniu programu z przekazanymi argumentami, zobaczysz następujące wyjście:

```
Argument 0 to program.exe.
Argument 1 to argument1.
Argument 2 to argument2.
```

## Wnikliwa analiza

Czytanie argumentów wiersza poleceń jest przydatne w przypadku, gdy chcemy, aby nasz program mógł pobierać dane od użytkownika bezpośrednio z linii poleceń. Argumenty te mogą również przekazywać informacje lub opcje konfiguracyjne do programu.

Warto pamiętać, że argument 0, czyli pierwszy element w tablicy argv, zawsze jest nazwą programu. W przykładzie powyżej otrzymaliśmy dwa przekazane argumenty, które były przechowywane w elemencie 1 i 2 tablicy.

## Zobacz także

Jeśli chcesz dowiedzieć się więcej o argumentach wiersza poleceń w C, polecamy zapoznać się z poniższymi źródłami:

- [Dokumentacja funkcji main w C](https://en.cppreference.com/w/c/language/main_function)
- [Podstawy obsługi argumentów wiersza poleceń w C](https://www.geeksforgeeks.org/command-line-arguments-in-c-cpp/)
- [Zastosowanie argumentów wiersza poleceń w praktycznych przykładach](http://www.zentut.com/c-tutorial/c-command-line-arguments/)