---
title:                "Czytanie argumentów linii poleceń"
html_title:           "Bash: Czytanie argumentów linii poleceń"
simple_title:         "Czytanie argumentów linii poleceń"
programming_language: "C"
category:             "C"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/c/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Co To Jest I Dlaczego?

Czytanie argumentów linii komend to proces, w którym program w języku C interpretuje argumenty przekazywane podczas uruchamiania. Programiści robią to, aby dostosować zachowanie programu na podstawie wprowadzonych przez użytkownika danych zewnętrznych.

## Jak To Zrobić:

```C
#include <stdio.h>

int main(int argc, char *argv[]) {
    int number;
    if (argc > 1) {
        sscanf(argv[1], "%d", &number);
        printf("Wprowadzona liczba to: %d\n", number);
    } else {
        printf("Nie dodano argumentu.\n");
    }
    return 0;
}
```

Na przykład, gdy program jest uruchamiany w ten sposób: `./program 5`, wydrukuje: `Wprowadzona liczba to: 5`.

## Szerzej:

- Kontekst historyczny: W dziwnych, przestarzałych językach, jak na przykład B, funkcja main nie miała argumentów. Powstanie C, które wprowadziło wsparcie dla argumentów linii komend było przełomem.

- Alternatywy: Możemy korzystać również z bibliotek zewnętrznych, takich jak `getopt` lub `argp`, które oferują rozbudowane funkcje do obsługi argumentów linii komend.

- Szczegóły implementacyjne: argv to tablica wskaźników do ciągów znaków, które reprezentują argumenty przekazane do programu. argc to liczba tych argumentów. argv[0] zawsze jest nazwą programu.

## Zobacz Również:

- [Dokumentacja GNU Getopt](https://www.gnu.org/software/libc/manual/html_node/Getopt.html)
- [Dokumentacja GNU Argp](https://www.gnu.org/software/libc/manual/html_node/Argp.html)