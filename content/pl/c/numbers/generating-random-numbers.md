---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 17:57:18.964016-07:00
description: "Jak to zrobi\u0107: W j\u0119zyku C losowe liczby mo\u017Cna generowa\u0107\
  \ za pomoc\u0105 funkcji `rand()`, kt\xF3ra jest cz\u0119\u015Bci\u0105 standardowej\
  \ biblioteki C `<stdlib.h>`. Domy\u015Blnie\u2026"
lastmod: '2024-03-13T22:44:35.881862-06:00'
model: gpt-4-0125-preview
summary: "W j\u0119zyku C losowe liczby mo\u017Cna generowa\u0107 za pomoc\u0105 funkcji\
  \ `rand()`, kt\xF3ra jest cz\u0119\u015Bci\u0105 standardowej biblioteki C `<stdlib.h>`."
title: Generowanie liczb losowych
weight: 12
---

## Jak to zrobić:
W języku C losowe liczby można generować za pomocą funkcji `rand()`, która jest częścią standardowej biblioteki C `<stdlib.h>`. Domyślnie `rand()` produkuje liczby pseudolosowe w zakresie od 0 do `RAND_MAX` (stała zdefiniowana w `<stdlib.h>`). Aby mieć więcej kontroli nad zakresem, programiści mogą manipulować wynikiem funkcji `rand()`.

Oto prosty przykład generowania losowej liczby między 0 a 99:

```c
#include <stdio.h>
#include <stdlib.h> // Dla rand() i srand()
#include <time.h>   // Dla time()

int main() {
    // Zasiew generatora liczb losowych
    srand((unsigned) time(NULL));

    // Generowanie losowej liczby między 0 a 99
    int randomNumber = rand() % 100;

    printf("Random Number: %d\n", randomNumber);

    return 0;
}
```

Przykładowy wynik może się różnić za każdym razem, gdy uruchamiasz ten program:

```
Random Number: 42
```
Aby generować losowe liczby w innym zakresie, możesz odpowiednio dostosować operator modulo (`%`). Na przykład, `rand() % 10` generuje liczby od 0 do 9.

Ważne jest, aby zauważyć, że zasiewanie generatora pseudolosowego (`srand()`) bieżącym czasem (`time(NULL)`) zapewnia różne sekwencje liczb losowych przy różnych wykonaniach programu. Bez zasiewania (`srand()`), `rand()` wyprodukowałaby tę samą sekwencję liczb za każdym razem, gdy program jest uruchamiany.

## Zagłębienie się
Funkcja `rand()` oraz jej odpowiednik do zasiewania `srand()` są częścią standardowej biblioteki C od dziesięcioleci. Oparte są na algorytmach, które generują sekwencje liczb, które tylko wydają się być losowe – stąd termin "pseudolosowe". Podstawowy algorytm w `rand()` to zazwyczaj generator liniowy kongruentny (LCG).

Chociaż `rand()` i `srand()` są wystarczające dla wielu aplikacji, mają znane ograniczenia, szczególnie dotyczące jakości losowości i potencjalnej przewidywalności. W przypadku aplikacji wymagających wysokiej jakości losowości, takich jak operacje kryptograficzne, należy rozważyć alternatywy takie jak `/dev/random` lub `/dev/urandom` (w systemach podobnych do Unix), lub API dostarczane przez biblioteki kryptograficzne.

Z wprowadzeniem C11, standard ISO C zawierał nowy nagłówek, `<stdatomic.h>`, oferujący bardziej wyrafinowaną kontrolę nad operacjami współbieżnymi, ale nie dotyczący bezpośrednio losowości. Dla prawdziwej losowości w C, deweloperzy często zwracają się ku specyficznym dla platformy lub zewnętrznym bibliotekom, które oferują lepsze algorytmy lub korzystają z źródeł entropii sprzętowej.

Pamiętaj, że chociaż `rand()` służy jako proste i dostępne środki do generowania liczb pseudolosowych, jego zastosowania w nowoczesnych aplikacjach są ograniczone przez jakość i przewidywalność jego wyników. Kiedy wymagane są bardziej solidne rozwiązania, zwłaszcza w aplikacjach wymagających bezpieczeństwa, zdecydowanie zaleca się szukanie rozwiązań poza standardową biblioteką.
