---
title:                "Generowanie liczb losowych"
date:                  2024-01-27T20:33:09.426081-07:00
model:                 gpt-4-0125-preview
simple_title:         "Generowanie liczb losowych"

tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/c/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Co i dlaczego?

Generowanie liczb losowych w języku C polega na tworzeniu ciągów liczb, które nie wykazują żadnego rozpoznawalnego wzorca, naśladując koncepcję losowości. Programiści wykorzystują liczby losowe do wielu celów, w tym do symulowania danych, zastosowań kryptograficznych oraz rozwoju gier, co czyni to kluczowym aspektem programowania.

## Jak to zrobić:

Aby generować liczby losowe w C, zwykle korzysta się z funkcji `rand()` znajdującej się w `stdlib.h`. Jednak kluczowe jest zainicjowanie generatora liczb losowych wartością początkową (nazywaną seed), aby zapewnić zróżnicowanie generowanych liczb między różnymi wykonaniami programu. Funkcja `srand()`, zainicjowana wartością, często obecnym czasem, ułatwia to.

Oto prosty przykład generowania liczby losowej od 0 do 99:

```c
#include <stdio.h>
#include <stdlib.h>
#include <time.h>

int main() {
    // Zainicjuj generator liczb losowych
    srand((unsigned) time(NULL));

    // Wygeneruj liczbę losową między 0 a 99
    int randomNumber = rand() % 100;

    // Wypisz wylosowaną liczbę
    printf("Wylosowana liczba: %d\n", randomNumber);

    return 0;
}
```

Przykładowe wyjście:

```
Wylosowana liczba: 42
```

Ważne jest, aby zauważyć, że każde wykonanie tego programu wyprodukuje nową losową liczbę, dzięki inicjowaniu obecnym czasem.

## Dogłębna analiza

Tradycyjny sposób generowania liczb losowych w C, za pomocą funkcji `rand()` i `srand()`, nie jest naprawdę losowy. Jest pseudolosowy. Jest to wystarczające dla wielu zastosowań, ale nie w sytuacjach wymagających wysokiego stopnia losowości, jak na przykład w poważnych zastosowaniach kryptograficznych. Sekwencja generowana przez `rand()` jest całkowicie określona przez ziarno dostarczone do `srand()`. Tak więc, jeśli ziarno jest znane, sekwencja może być przewidywana, co redukuje losowość.

Historycznie, funkcję `rand()` krytykowano za jej niską jakość losowości i ograniczony zasięg. Nowoczesne alternatywy obejmują korzystanie ze specyficznych dla urządzenia API lub zewnętrznych bibliotek, które lepiej przybliżają prawdziwą losowość lub, w systemach typu UNIX, czytanie z `/dev/random` lub `/dev/urandom` w celach kryptograficznych.

Na przykład, korzystanie z `/dev/urandom` w C:

```c
#include <stdio.h>
#include <stdlib.h>

int main() {
    FILE *fp;
    unsigned int randomNumber;

    // Otwórz /dev/urandom do odczytu
    fp = fopen("/dev/urandom", "r");

    // Przeczytaj liczbę losową
    fread(&randomNumber, sizeof(randomNumber), 1, fp);

    // Wypisz wylosowaną liczbę
    printf("Wylosowana liczba: %u\n", randomNumber);

    // Zamknij plik
    fclose(fp);

    return 0;
}
```

Ta metoda czyta bezpośrednio z puli entropii systemu, oferując wyższą jakość losowości odpowiednią dla bardziej wrażliwych zastosowań. Jednak podejście to może stwarzać problemy z przenośnością na różne platformy, co czyni je mniej uniwersalnym niż korzystanie z `rand()`.

Bez względu na metodę, zrozumienie natury losowości i jej implementacji w C jest kluczowe dla opracowywania skutecznych, bezpiecznych i angażujących aplikacji.
