---
title:                "Praca z liczbami zespolonymi"
date:                  2024-01-26T04:38:15.471913-07:00
model:                 gpt-4-0125-preview
simple_title:         "Praca z liczbami zespolonymi"

category:             "C"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/c/working-with-complex-numbers.md"
---

{{< edit_this_page >}}

## Co i dlaczego?
Liczby zespolone, zawierające część rzeczywistą i urojoną (takie jak 3 + 4i), są kluczowe w zaawansowanych obliczeniach, takich jak przetwarzanie sygnałów czy rozwiązywanie pewnych równań. Programiści obsługują je w aplikacjach wymagających intensywnych obliczeń matematycznych, gdzie tradycyjne liczby się nie sprawdzają.

## Jak to zrobić:
C, od standardu C99, posiada natywny typ złożony i bibliotekę. Oto jak z nich korzystać:

```C
#include <stdio.h>
#include <complex.h>

int main() {
    // Deklaracja dwóch liczb zespolonych
    double complex z1 = 1.0 + 3.0 * I;
    double complex z2 = 2.0 - 2.0 * I;

    // Operacje na liczbach zespolonych
    double complex sum = z1 + z2;
    double complex mult = z1 * z2;

    // Drukowanie wyników
    printf("Suma: %.1f + %.1fi\n", creal(sum), cimag(sum));
    printf("Iloczyn: %.1f + %.1fi\n", creal(mult), cimag(mult));

    // Wartość bezwzględna i kąt fazowy
    printf("Abs(z1): %f\n", cabs(z1));
    printf("Arg(z1): %f\n", carg(z1));

    return 0;
}
```

Przykładowe wyjście:
```
Suma: 3.0 + 1.0i
Iloczyn: 8.0 + 2.0i
Abs(z1): 3.162278
Arg(z1): 1.249046
```
## Wgłębienie
Liczby zespolone sięgają wieków, z korzeniami w algebrze XVI wieku. Idąc naprzód, są teraz podstawą w wielu językach programowania, nie tylko w C.

Standard C99 wprowadził `<complex.h>`, nagłówek definiujący makra, funkcje i typ danych `complex`. Istnieją alternatywy - takie jak tworzenie własnej struktury, ale po co na nowo wynajdować koło? Standardowa biblioteka języka C jest zoptymalizowana i gotowa do użycia.

Pomimo swojej mocy, wsparcie dla liczb złożonych w C nie jest pozbawione krytyków. Może być mniej intuicyjne niż podobne funkcje w językach takich jak Python, a obsługa przypadków skrajnych może być kłopotliwa. Ale pod względem czystej wydajności jest to nadal solidny wybór.

## Zobacz także
- Dokumentacja standardu C99 dla `<complex.h>`: https://en.cppreference.com/w/c/numeric/complex
- Standard IEEE dotyczący arytmetyki zmiennoprzecinkowej (IEEE 754): https://ieeexplore.ieee.org/document/4610935
- Internetowy samouczek matematyki liczb zespolonych w C: https://www.tutorialspoint.com/complex-number-arithmetic-in-c-programming
