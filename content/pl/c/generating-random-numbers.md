---
title:                "Generowanie liczb losowych"
date:                  2024-01-20T17:48:32.163607-07:00
model:                 gpt-4-1106-preview
simple_title:         "Generowanie liczb losowych"
programming_language: "C"
category:             "C"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/c/generating-random-numbers.md"
---

{{< edit_this_page >}}

## What & Why? (Co i dlaczego?)
Generowanie liczb losowych to proces tworzenia numerów, które nie przewidują kolejnych. Programiści używają ich do testowania, symulacji i gier. 

## How to: (Jak to zrobić:)
```C
#include <stdio.h>
#include <stdlib.h>
#include <time.h>

int main() {
    // Inicjalizacja generatora liczb losowych
    srand((unsigned int)time(NULL));

    // Generowanie i wyświetlanie 5 liczb losowych
    for(int i = 0; i < 5; i++) {
        printf("%d ", rand() % 50); // Liczby z zakresu 0-49
    }

    return 0;
}
```
Wyjście przykładowe:
```
23 12 45 7 39
```

## Deep Dive (Głębsze zanurzenie)
Generowanie liczb losowych w C zaczyna się od funkcji `rand()`, czyli części standardowej biblioteki. Zaczęło się w latach 70., gdy RAND Corporation stworzyła jedne z pierwszych metod. Alternatywą jest `/dev/random` w Unix, ale to mniej przenośne i wolniejsze. `rand()` działa szybko, lecz dla kryptografii jest za słabe – zamiast tego używamy `openssl` lub specjalizowane biblioteki.

Implementacja `rand()` w C nie jest idealnie losowa – to tzw. generator pseudolosowy. Zależy od "ziarna" (`seed`), więc używamy `srand()` z różnymi ziarnami, np. obecnym czasem (`time(NULL)`), by wyniki były bardziej nieprzewidywalne.

## See Also (Zobacz również)
- [cplusplus.com: rand](http://www.cplusplus.com/reference/cstdlib/rand/)
- [cplusplus.com: srand](http://www.cplusplus.com/reference/cstdlib/srand/)
- [Dokumentacja OpenSSL dla generatorów losowych](https://www.openssl.org/docs/manmaster/man3/RAND_bytes.html)
