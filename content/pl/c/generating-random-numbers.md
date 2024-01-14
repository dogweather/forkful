---
title:    "C: Generowanie losowych liczb"
keywords: ["C"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/c/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Dlaczego

Generowanie losowych liczb jest ważną częścią programowania w języku C. Losowe liczby są wykorzystywane w wielu aplikacjach, takich jak gry, symulacje, testowanie i wiele innych. W tym artykule dowiesz się jak wygenerować losowe liczby w Twoim programie w języku C.

## Jak to zrobić

Istnieje kilka sposobów na wygenerowanie losowych liczb w języku C. Jednym z nich jest użycie funkcji `rand()` w połączeniu z funkcją `srand()`, która ustawia ziarno dla generatora liczb pseudolosowych. Poniżej znajduje się kod, który wygeneruje 10 losowych liczb z zakresu od 1 do 10:

```C
#include <stdio.h>
#include <stdlib.h>
#include <time.h>

int main() {
    // ustawienie ziarna dla generatora liczb pseudolosowych
    srand(time(0));

    for (int i = 0; i < 10; i++) {
        // wygenerowanie losowej liczby z zakresu 1-10 i wyświetlenie jej na ekranie
        int random_number = rand() % 10 + 1; 
        printf("%d ", random_number);
    }

    return 0;
}

```

Przykładowy wynik:

```
7 2 9 4 1 8 10 5 3 6
```

Możesz również użyć funkcji `random()` z biblioteki `stdlib.h` do generowania liczb pseudolosowych. Metoda ta działa na podobnej zasadzie jak `rand()`, ale może dawać lepsze wyniki w niektórych przypadkach. Poniżej znajduje się przykładowy kod wykorzystujący `random()`:

```C
#include <stdio.h>
#include <stdlib.h>
#include <time.h>

int main() {
    // ustawienie ziarna dla generatora liczb pseudolosowych
    srand(time(0));

    for (int i = 0; i < 10; i++) {
        // wygenerowanie losowej liczby z zakresu 1-10 i wyświetlenie jej na ekranie
        int random_number = (random() % 10) + 1; 
        printf("%d ", random_number);
    }

    return 0;
}
```

Przykładowy wynik:

```
8 1 7 3 9 2 10 4 5 6
```

## Deep Dive

Generowanie liczb pseudolosowych jest w rzeczywistości generowaniem ciągu liczb w sposób deterministyczny, tzn. na podstawie określonych reguł. W przypadku funkcji `rand()` i `random()` używana jest metoda zwana liniowym kongruencji. Jest to prosta i szybka metoda, ale może dawać nieco przewidywalne wyniki, zwłaszcza gdy nie jest używana w odpowiedni sposób.

Aby uzyskać lepszą losowość, można wykorzystać bardziej złożone metody generowania liczb pseudolosowych, takie jak generator Mersenne Twister czy KISS (Keep It Simple Stupid) RNG. Te metody wymagają jednak dodatkowych bibliotek i są bardziej skomplikowane w użyciu.

## Zobacz także

- [Dokumentacja funkcji rand()](https://www.tutorialspoint.com/c_standard_library/c_function_rand.htm)
- [Dokumentacja funkcji random()](https://www.tutorialspoint.com/c_standard_library/c_function_random.htm)
- [Metoda liniowego kongruencji](https://pl.wikipedia.org/wiki/Generator_liczb_pseudolosowych#Metoda_liniowego_kongruencji)
- [KISS RNG](https://pl.wikipedia.org/wiki/KISS_(Generator_liczb_pseudolosowych))
- [Generator Mersenne Twister](https://pl.wikipedia.org/wiki/Generator_Mersenne_Twister)