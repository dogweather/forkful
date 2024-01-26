---
title:                "Odczytywanie argumentów linii poleceń"
date:                  2024-01-20T17:55:36.949979-07:00
model:                 gpt-4-1106-preview
simple_title:         "Odczytywanie argumentów linii poleceń"
programming_language: "C"
category:             "C"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/c/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## What & Why? (Co i dlaczego?)
Czytanie argumentów linii poleceń to sposób, aby Twoja aplikacja w C przyjmowała dane od użytkownika już przy starcie. Programiści korzystają z tej techniki, by elastycznie dostosowywać działanie programu bez potrzeby tworzenia nowego interfejsu użytkownika.

## How to: (Jak to zrobić:)
Często używamy `argc` i `argv` w funkcji `main()` do obsługi argumentów. `argc` to liczba argumentów, `argv` to tablica ciągów znaków (stringów) zawierających rzeczywiste argumenty.

```C
#include <stdio.h>

int main(int argc, char *argv[]) {
    printf("Nazwa programu: %s\n", argv[0]);
    for (int i = 1; i < argc; i++) {
        printf("Argument %d: %s\n", i, argv[i]);
    }
    return 0;
}
```

Uruchamiając program `./prog -o plik.txt -v`, otrzymamy:

```
Nazwa programu: ./prog
Argument 1: -o
Argument 2: plik.txt
Argument 3: -v
```

## Deep Dive (Dogłębna analiza):
Historia: Początki C sięgają lat 70., już wtedy obsługa argumentów była standardem. Alternatywy? Możesz użyć `getopt()` lub `getopt_long()` do bardziej zaawansowanej obsługi argumentów. Implementacja: Funkcja `main()` w C może być zdefiniowana z `argc` i `argv`, gdzie `argc` zawiera liczbę argumentów przekazanych do programu, a `argv` to wskaźnik na tablicę ciągów znaków reprezentujących te argumenty. Pierwszy argument (`argv[0]`) to zazwyczaj nazwa uruchomionego programu.

## See Also (Zobacz także):
- [GNU C Library: Program Arguments](https://www.gnu.org/software/libc/manual/html_node/Program-Arguments.html)
- [Using the getopt() function](https://www.ibm.com/docs/en/zos/2.2.0?topic=functions-getopt-parse-command-line-options)
