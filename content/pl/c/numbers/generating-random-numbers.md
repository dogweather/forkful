---
aliases:
- /pl/c/generating-random-numbers/
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 17:57:18.964016-07:00
description: "Generowanie losowych liczb w j\u0119zyku C polega na tworzeniu warto\u015B\
  ci, kt\xF3re s\u0105 nieprzewidywalne i pod\u0105\u017Caj\u0105 za okre\u015Blonym\
  \ rozk\u0142adem, takim jak jednostajny\u2026"
lastmod: 2024-02-18 23:08:50.075455
model: gpt-4-0125-preview
summary: "Generowanie losowych liczb w j\u0119zyku C polega na tworzeniu warto\u015B\
  ci, kt\xF3re s\u0105 nieprzewidywalne i pod\u0105\u017Caj\u0105 za okre\u015Blonym\
  \ rozk\u0142adem, takim jak jednostajny\u2026"
title: Generowanie liczb losowych
---

{{< edit_this_page >}}

## Co i dlaczego?

Generowanie losowych liczb w języku C polega na tworzeniu wartości, które są nieprzewidywalne i podążają za określonym rozkładem, takim jak jednostajny lub normalny. Ta zdolność jest kluczowa dla zastosowań od symulacji i gier po operacje kryptograficzne, gdzie nieprzewidywalność lub symulacja rzeczywistościowej losowości jest niezbędna.

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
