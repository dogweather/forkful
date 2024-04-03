---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 18:14:08.294353-07:00
description: "Liczby zespolone sk\u0142adaj\u0105 si\u0119 z cz\u0119\u015Bci rzeczywistej\
  \ i urojonej, reprezentowane jako `a + bi`, gdzie `i` jest pierwiastkiem kwadratowym\
  \ z `-1`. Programi\u015Bci\u2026"
lastmod: '2024-03-13T22:44:35.879665-06:00'
model: gpt-4-0125-preview
summary: "Liczby zespolone sk\u0142adaj\u0105 si\u0119 z cz\u0119\u015Bci rzeczywistej\
  \ i urojonej, reprezentowane jako `a + bi`, gdzie `i` jest pierwiastkiem kwadratowym\
  \ z `-1`."
title: Praca z liczbami zespolonymi
weight: 14
---

## Co i dlaczego?

Liczby zespolone składają się z części rzeczywistej i urojonej, reprezentowane jako `a + bi`, gdzie `i` jest pierwiastkiem kwadratowym z `-1`. Programiści pracują z liczbami zespolonymi w różnych dziedzinach, takich jak inżynieria elektryczna, informatyka kwantowa i dynamika płynów, wykorzystując ich unikalne właściwości do symulacji, przetwarzania sygnałów i rozwiązywania określonych rodzajów równań matematycznych.

## Jak to zrobić:

W języku C liczby zespolone są obsługiwane przez bibliotekę standardową, a konkretnie przez `<complex.h>`. Aby z nich korzystać, należy deklarować zmienne typu `double complex` (lub `float complex` dla pojedynczej precyzji). Oto jak wykonać podstawowe operacje:

```c
#include <stdio.h>
#include <complex.h>

int main() {
    double complex z1 = 1.0 + 2.0*I; // Deklaracja liczby zespolonej 1+2i
    double complex z2 = 1.0 - 2.0*I; // Deklaracja innej liczby zespolonej 1-2i

    // Dodawanie
    double complex suma = z1 + z2;
    printf("Suma: %.2f + %.2fi\n", creal(suma), cimag(suma)); // Wynik: Suma: 2.00 + 0.00i

    // Mnożenie
    double complex iloczyn = z1 * z2;
    printf("Iloczyn: %.2f + %.2fi\n", creal(iloczyn), cimag(iloczyn)); // Wynik: Iloczyn: 5.00 + 0.00i

    // Sprzężenie zespolone
    double complex sprzezenie = conj(z1);
    printf("Sprzężenie z1: %.2f + %.2fi\n", creal(sprzezenie), cimag(sprzezenie)); // Wynik: Sprzężenie z1: 1.00 - 2.00i

    // Moduł
    double modul = cabs(z1);
    printf("Moduł z1: %.2f\n", modul); // Wynik: Moduł z1: 2.24

    // Argument (faza)
    double faza = carg(z1);
    printf("Faza z1: %.2f\n", faza); // Wynik podany w radianach

    return 0;
}
```
Zwróć uwagę, że `I` jest stałą reprezentującą jednostkę urojoną w `<complex.h>`. Funkcje takie jak `creal()` i `cimag()` pozwalają na ekstrakcję części rzeczywistej i urojonej odpowiednio, podczas gdy `conj()` oblicza sprzężenie zespolone. Do obliczania modułu i argumentu (fazy) liczb zespolonych używane są `cabs()` i `carg()`.

## Wnikliwe spojrzenie

Obsługa liczb zespolonych w C jest stosunkowo nowa, została znormalizowana w C99. Przed tym, arytmetyka liczb zespolonych w C była uciążliwa, często wymagająca niestandardowych struktur danych i funkcji. Włączenie `<complex.h>` i typów danych zespolonych znacząco zwiększyło możliwości języka dla zastosowań naukowych i inżynierskich. Warto jednak zauważyć, że niektóre języki, takie jak Python, oferują bardziej intuicyjne wsparcie dla liczb zespolonych poprzez wbudowane typy danych i bogatszy zestaw funkcji bibliotecznych. Pomimo to, wydajność i kontrola oferowana przez C czynią go preferowanym wyborem dla zadań obliczeń wysokiej wydajności, nawet jeśli oznacza to radzenie sobie z nieco bardziej rozbudowaną składnią dla arytmetyki zespolonej.
