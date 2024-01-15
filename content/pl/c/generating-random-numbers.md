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

## Dlaczego

Losowe liczby są niezbędne w wielu programach. Możesz ich użyć do testowania swojego kodu, symulacji rzeczywistych warunków lub do stworzenia prostych gier lub aplikacji.

## Jak to zrobić

Aby wygenerować losową liczbę w języku C, możesz skorzystać z funkcji `rand()` z biblioteki `stdlib.h`. Należy jednak pamiętać, że ta funkcja generuje liczbę z zakresu od 0 do `RAND_MAX`. Aby zawęzić ten zakres, możesz wykorzystać proste działania matematyczne, np. aby otrzymać wartość z zakresu od 1 do 10, możesz użyć wyrażenia `rand() % 10 + 1`.

```C
#include <stdio.h>
#include <stdlib.h>
#include <time.h>

int main() {
  // ustawienie ziarna dla funkcji srand()
  srand(time(0));
  
  // wygenerowanie losowej liczby z zakresu od 1 do 10
  int random_num = rand() % 10 + 1;
  
  // wyświetlenie wyniku
  printf("Wylosowana liczba to: %d\n", random_num);

  return 0;
}
```

**Wyjście:**

`Wylosowana liczba to: 7`

## Deep Dive

Jedną z bibliotek, którą warto wykorzystać przy generowaniu losowych liczb w języku C, jest `random`. Poza funkcją `rand()`, oferuje ona także bardziej zaawansowane metody, takie jak `random()` czy `srandom()`, które pozwalają na bardziej precyzyjną kontrolę nad wygenerowanymi liczbami.

Ponadto, dla bezpiecznego generowania liczb losowych zaleca się użycie `random()` zamiast `rand()`, ponieważ pochodzi ona zgodnego z normami generatora liczb losowych.

## Zobacz również

- [Dokumentacja biblioteki `stdlib.h` w języku C](https://www.tutorialspoint.com/c_standard_library/stdlib_h.htm)
- [Tutorial o generowaniu liczb losowych w języku C](https://www.geeksforgeeks.org/generating-random-number-range-c/)
- [Dokumentacja biblioteki `random` w języku C](https://www.gnu.org/software/libc/manual/html_node/Random-Number-Functions.html#Random-Number-Functions)