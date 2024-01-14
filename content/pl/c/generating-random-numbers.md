---
title:                "C: Generowanie losowych liczb"
programming_language: "C"
category:             "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/c/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Dlaczego generowanie liczb losowych jest ważne

Generowanie liczb losowych jest ważną częścią programowania, ponieważ może być używane w różnych celach. Na przykład, może być wykorzystywane w grach wideo do losowego generowania przeciwników lub przedmiotów, w kryptografii do generowania kluczy szyfrujących, w testowaniu jednostkowym do generowania różnych wejść, a także w symulacjach naukowych. Dlatego też, umiejętność generowania liczb losowych jest ważna dla każdego programisty.

## Jak to zrobić

Aby wygenerować liczbę losową w języku C, należy użyć funkcji `rand()` oraz `srand()` z biblioteki standardowej `stdlib.h`. Poniżej znajduje się przykładowy kod, który wygeneruje 10 liczb losowych z zakresu od 1 do 100 i wyświetli je na ekranie:

```C
#include <stdio.h>
#include <stdlib.h>

int main()
{
    // ustawienie ziarna generatora pseudo-losowego
    srand(time(NULL));

    int i;
    for(i = 0; i < 10; i++)
    {
        // wygenerowanie liczby losowej i wyświetlenie jej na ekranie
        int random = rand() % 100 + 1;
        printf("%d\n", random);
    }

    return 0;
}
```

Przykładowy wynik:

```
56
78
2
93
24
42
17
99
10
89
```

## Głębsze zagadnienia

Podczas generowania liczb losowych warto pamiętać, że są one generowane w sposób pseudo-losowy, co oznacza, że można przewidzieć kolejną liczbę w oparciu o poprzednie. Dlatego też, ziarno generatora powinno być ustawione za pomocą funkcji `srand()` w oparciu o zmieniający się czas lub inne wartości, aby uzyskać bardziej losowe wyniki.

Kolejnym ważnym aspektem jest wybór odpowiedniego algorytmu generującego. Biblioteka `stdlib.h` używa standardowego algorytmu nazywanego "linear congruential generator" do generowania liczb losowych, ale istnieją lepsze i bezpieczniejsze alternatywy, takie jak Mersenne Twister czy Xorshift.

Warto również zwrócić uwagę na rozkład wygenerowanych liczb. Domyslnie, funkcja `rand()` zwraca liczby z rozkładu jednostajnego, co oznacza, że każda liczba ma taką samą szansę na wylosowanie. W niektórych przypadkach może jednak być potrzebny inny rodzaj rozkładu, na przykład normalny. Wtedy należy użyć odpowiednich funkcji z biblioteki `math.h` do przekształcenia liczby losowej z jednego rozkładu na drugi.

## Zostańmy w kontakcie

Teraz, gdy znasz podstawy generowania liczb losowych w języku C, możesz je wykorzystać w swoich projektach. Pamiętaj jednak o ustawianiu ziarna generatora w sposób bezpieczny i wybieraniu odpowiedniego algorytmu i rozkładu dla swoich potrzeb.

Zobacz także:

- [Dokumentacja funkcji rand() w języku C](https://www.tutorialspoint.com/c_standard_library/c_function_rand.htm)
- [Alternatywne algorytmy generujące w C](https://www.geeksforgeeks.org/rand-and-srand-in-ccpp/)
- [Wyjaśnienie podstawowych koncepcji generowania liczb losowych](https://www.eetimes.com/basics-of-random-number-generation/)