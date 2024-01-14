---
title:                "C: Tworzenie losowych liczb"
simple_title:         "Tworzenie losowych liczb"
programming_language: "C"
category:             "C"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/c/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Dlaczego

Generowanie liczb losowych jest niezwykle ważnym aspektem programowania w języku C. Pozwala nam ono na tworzenie losowych danych, które są niezbędne w wielu różnych zastosowaniach, takich jak gry komputerowe, symulacje czy również testowanie oprogramowania.

## Jak to zrobić 

Aby generować liczby losowe w języku C, musimy skorzystać z funkcji `rand()`. Przed jej użyciem musimy również zainicjować generator liczb losowych za pomocą funkcji `srand()`. Poniżej znajdują się przykładowy kod oraz wynik działania programu, który wygeneruje 10 liczb losowych z zakresu od 1 do 100:

```C
#include <stdio.h>
#include <stdlib.h>
#include <time.h>

int main(){
    // inicjalizacja generatora liczb losowych
    srand(time(0));

    // generowanie 10 liczb losowych i wyświetlenie ich na ekranie
    for(int i = 0; i < 10; i++){
        // użycie funkcji rand() do wygenerowania liczby z zakresu [0, RAND_MAX]
        int random_number = rand();
        // wykorzystanie operatora modulo do sprowadzenia liczby do zakresu [1, 100]
        random_number = random_number % 100 + 1;
        printf("%d\n", random_number);
    }

    return 0;
}
```

Przykładowy wynik działania programu:
```
65
99
42
78
23
96
8
10
57
32
```

## Głębsze zagłębianie

Funkcja `rand()` w języku C generuje liczby z zakresu [0, RAND_MAX], gdzie RAND_MAX jest stałą określoną przez kompilator. Zazwyczaj jest to liczba 32767 lub 2147483647. Aby uzyskać liczby z określonego zakresu, musimy wykorzystać odpowiednie operatory arytmetyczne, takie jak mnożenie, dzielenie czy modulo.

Warto również wspomnieć, że funkcja `rand()` generuje liczby pseudolosowe, co oznacza, że są one wygenerowane za pomocą deterministycznego algorytmu. W związku z tym, jeśli zostanie użyta ta sama wartość do inicjalizacji generatora liczb losowych, wygenerowany zostanie ten sam ciąg liczb. Dlatego też przed użyciem funkcji `rand()`, zaleca się zainicjowanie generatora za pomocą różnych wartości, na przykład aktualnego czasu.

## Zobacz także

- [Dokumentacja języka C - funkcja rand()](https://www.tutorialspoint.com/c_standard_library/c_function_rand.htm)
- [Porównanie metod generowania liczb losowych w języku C](https://www.techonthenet.com/c_language/standard_library_functions/rand.php)
- [Książka "The C Programming Language" autorstwa Dennisa Ritchie'go i Briana Kernighana](https://www.goodreads.com/book/show/515601.The_C_Programming_Language)