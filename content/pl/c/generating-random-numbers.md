---
title:                "Generowanie losowych liczb"
html_title:           "C: Generowanie losowych liczb"
simple_title:         "Generowanie losowych liczb"
programming_language: "C"
category:             "C"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/c/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Czym to jest i po co to robić?
Generowanie liczb losowych jest podstawowym elementem programowania, który pozwala na stworzenie symulacji zdarzeń losowych. Programiści często używają generatorów liczb losowych do testowania kodu i tworzenia aplikacji, które wymagają losowego wyboru lub generowania wartości.

## Jak to zrobić?
Możesz wygenerować losowe liczby za pomocą funkcji `rand()`, która zwraca liczbę całkowitą z zakresu od 0 do `RAND_MAX`. Aby określić zakres dla generowanych liczb, użyj funkcji `srand()` z argumentem określającym ziarno lub ustaw generator w tryb który gwarantuje unikalność wygenerowanych liczb.

```
#include <stdio.h>
#include <stdlib.h>

int main()
{
  int i;
  
  // Ustaw ziarno generatora
  srand(100);
  
  // Wygeneruj i wyświetl 5 liczb losowych z zakresu 0-9
  for(i = 0; i < 5; i++)
  {
    printf("%d\n", rand() % 10);
  }
  
  return 0;
}
```

Przykładowy wynik:
```
2
0
5
6
8
```

## Głębszy zanurzenie
Funkcja `rand()` została wprowadzona w języku C w 1972 roku i od tego czasu jest niezastąpiona w tworzeniu symulacji i gier. Wraz z rozwojem technologii, powstały jednak alternatywne metody generowania liczb losowych, takie jak np. generator liczb pseudolosowych oparty na algorytmie RSA.

## Zobacz też
- [Dokumentacja funkcji rand()](https://www.cplusplus.com/reference/cstdlib/rand/)
- [Dokumentacja funkcji srand()](https://www.cplusplus.com/reference/cstdlib/srand/)
- [Klasyfikacja generatorów liczb losowych](https://pl.wikipedia.org/wiki/Generator_liczb_pseudolosowych)