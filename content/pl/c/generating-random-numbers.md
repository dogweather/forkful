---
title:                "Generowanie liczb losowych"
html_title:           "Gleam: Generowanie liczb losowych"
simple_title:         "Generowanie liczb losowych"
programming_language: "C"
category:             "C"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/c/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Co & Dlaczego?

Generowanie losowych liczb to proces tworzenia liczb, które nie mają żadnej widocznej wzorowości. Programiści robią to, aby dodawać nieprzewidywalność do ich aplikacji, symulowań czy gier.

## Jak to zrobić:

```C
#include <stdio.h> 
#include <stdlib.h> 
#include <time.h> 

int main(){
   
   srand(time(0)); 
   int losowaLiczba = rand(); 

   printf("Losowa liczba: %d\n", losowaLiczba); 

   return 0;
}
```
Gdy uruchomisz ten kod, zobaczysz na wyjściu coś takiego:

```C
Losowa liczba: 172389184
```
Liczba ta będzie różna za każdym razem, gdy uruchomisz program. 

## Głębsze spojrzenie:

Generowanie liczb losowych ma swoje korzenie w starożytnych grach hazardowych, ale zastosowania dzisiejsze są bardziej techniczne. W C, `rand()` i `srand()` to popularne funkcje do generowania liczby losowej, ale są one pseudolosowe, czyli wydają się losowe, ale są produkowane przez deterministyczny algorytm.

Alternatywą jest użycie `/dev/random` lub `/dev/urandom` na systemach Unix, które oferują lepsze (choć wolniejsze) generowanie liczb losowych.

Zwróć uwagę, że `rand()` generuje liczby w określonym zakresie, więc często używa się go z operatorem modulo (`%`) aby ograniczyć zakres wyjściowych liczb losowych.

## Zobacz również:

1) "C Library - <stdlib.h>," TutorialsPoint, dostępne online: https://www.tutorialspoint.com/c_standard_library/stdlib_h.htm
2) "How to Generate a Random Number in C Programming," WikiHow, dostępne online: https://www.wikihow.com/Generate-a-Random-Number-in-C-Programming
3) "Generating random numbers," GNU, dostępne online: https://www.gnu.org/software/libc/manual/html_node/Random-Numbers.html
4) "Pseudorandom number generator," Wikipedia, dostępne online: https://en.wikipedia.org/wiki/Pseudorandom_number_generator