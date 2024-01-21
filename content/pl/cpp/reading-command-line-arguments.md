---
title:                "Odczytywanie argumentów linii poleceń"
date:                  2024-01-20T17:55:30.025837-07:00
model:                 gpt-4-1106-preview
simple_title:         "Odczytywanie argumentów linii poleceń"
programming_language: "C++"
category:             "C++"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/cpp/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## What & Why? (Co i dlaczego?)
Odczytywanie argumentów linii komend to sposób na przekazywanie danych zewnętrznych do programu przy jego starcie. Programiści wykorzystują to, aby dostosować działanie aplikacji bez interakcji z GUI lub plikami konfiguracyjnymi.

## How to: (Jak to zrobić:)
```C++
#include <iostream>

int main(int argc, char* argv[]) {
    std::cout << "You have entered " << argc << " arguments:" << std::endl;
    for (int i = 0; i < argc; ++i) {
        std::cout << i << ": " << argv[i] << std::endl;
    }
    return 0;
}
```
Przykładowe wyjście, gdy uruchamiasz `./program arg1 arg2 arg3`:
```
You have entered 4 arguments:
0: ./program
1: arg1
2: arg2
3: arg3
```

## Deep Dive (Dogłębna analiza):
Odczytywanie argumentów linii komend ma korzenie w początkach programowania. W C++, argumenty te są zazwyczaj przekazywane do funkcji `main()` jako `argc` (liczba argumentów) i `argv` (tablica argumentów). Metoda ta jest standardem POSIX i jest używana w różnych systemach oraz językach programowania.

Alternatywą jest użycie bibliotek, takich jak `boost::program_options`, które oferują przejrzysty interfejs do zarządzania złożoną logiką argumentów.

Jako szczegóły implementacyjne warto zaznaczyć, że `argv[0]` jest zawsze nazwą programu, a `argv[argc]` jest zawsze `NULL`, co może pomóc w projektowaniu bezpiecznych pętli.

## See Also (Zobacz również):
- [cppreference.com on "main function"](https://en.cppreference.com/w/cpp/language/main_function)
- [GNU documentation on "Program Arguments"](https://www.gnu.org/software/libc/manual/html_node/Program-Arguments.html)
- [Boost.Program_options library](https://www.boost.org/doc/libs/1_75_0/doc/html/program_options.html)