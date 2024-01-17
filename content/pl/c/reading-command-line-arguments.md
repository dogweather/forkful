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

## Co i Dlaczego?
Odczytywanie argumentów linii poleceń to proces, w którym programy pobierają dane przekazane przez użytkownika podczas uruchamiania z wiersza poleceń. Programiści wykonują tę czynność, aby dostosować zachowanie swojego programu do różnych scenariuszy lub aby uzyskać informacje od użytkownika.

## Jak to zrobić:
```C
#include <stdio.h>

int main(int argc, char *argv[]) {
    printf("Liczba argumentów: %d\n", argc);
    printf("Argumenty:\n");
    for (int i=0; i<argc; i++) {
        printf("%s\n", argv[i]);
    }
    return 0;
}
```
Przykładowe wyjście:
```bash
$ ./program arg1 arg2 arg3
Liczba argumentów: 4
Argumenty:
./program
arg1
arg2
arg3
```

## Wnikliwszy Przegląd:
Odczytywanie argumentów linii poleceń ma swoje korzenie w systemach operacyjnych UNIX, gdzie programy są uruchamiane z wiersza poleceń i mogą otrzymywać argumenty od użytkownika. Istnieje kilka alternatywnych sposobów na odczytywanie argumentów w języku C, w tym korzystanie z funkcji getopt lub argv[]. Programista może również zaimplementować własne rozwiązanie, jeśli chce dostosować sposób przekazywania argumentów do swoich potrzeb.

## Zobacz również:
- [Dokumentacja funkcji getopt w języku C](https://www.gnu.org/software/libc/manual/html_node/Getopt.html)
- [Przykłady odczytywania argumentów linii poleceń w C](https://www.cprogramming.com/tutorial/c/lesson14.html)