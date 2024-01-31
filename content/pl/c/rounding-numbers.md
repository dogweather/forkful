---
title:                "Zaokrąglanie liczb"
date:                  2024-01-26T03:43:23.940610-07:00
model:                 gpt-4-0125-preview
simple_title:         "Zaokrąglanie liczb"

tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/c/rounding-numbers.md"
---

{{< edit_this_page >}}

## Co i dlaczego?
Zaokrąglanie liczb polega na obcinaniu cyfr po pewnym miejscu, opcjonalnie dostosowując ostatnią zachowaną cyfrę. Programiści zaokrąglają, aby zredukować precyzję, kiedy dokładne wartości nie są konieczne, zarządzać błędami liczby zmiennoprzecinkowej lub przygotować numery do przyjaznego użytkownikowi wyświetlania.

## Jak to zrobić:
W C zazwyczaj używa się funkcji `floor()`, `ceil()` lub `round()`. Oto szybka prezentacja:

```C
#include <stdio.h>
#include <math.h>

int main() {
    double num = 3.14159;
    double num_floor = floor(num);
    double num_ceil = ceil(num);
    double num_round = round(num);

    printf("Floor: %.2f\n", num_floor); // Podłoga: 3.00
    printf("Ceil: %.2f\n", num_ceil);   // Sufit: 4.00
    printf("Round: %.2f\n", num_round); // Zaokrąglenie: 3.00
    return 0;
}
```

Dla większej kontroli, jak zaokrąglanie do konkretnego miejsca, mnoży się, zaokrągla i dzieli:

```C
double roundToPlace(double num, int place) {
    double scale = pow(10.0, place);
    return round(num * scale) / scale;
}

// ...

double num = 3.14159;
double num_rounded = roundToPlace(num, 2);
printf("Zaokrąglony do 2 miejsc po przecinku: %.2f\n", num_rounded); // Zaokrąglony do 2 miejsc po przecinku: 3.14
```

## Dogłębna analiza
Dawniej zaokrąglanie często oznaczało proces ręczny - ciężką pracę tylko z długopisem i papierem. Dzięki informatyzacji zautomatyzowaliśmy to, ale arytmetyka liczby zmiennoprzecinkowej wprowadziła niuanse ze względu na jej binarną naturę, gdzie niektóre liczby nie mogą być dokładnie reprezentowane.

Alternatywy dla standardowego zaokrąglania obejmują obcinanie (po prostu odrzucanie dodatkowych cyfr) lub zaokrąglanie bankerskie, które zaokrągla do najbliższej parzystej liczby, gdy wartość jest dokładnie pomiędzy dwiema wartościami, redukując stronniczość w powtarzających się obliczeniach.

Implementacja staje się trudna, gdy trzeba zaokrąglić liczby o dowolnej precyzji lub obsługiwać specjalne przypadki, takie jak nieskończoność, sygnalizujące NaNy lub wartości subnormalne. Funkcje biblioteki standardowej C obsługują podstawy, ale jeśli potrzebujesz zaokrąglać dziesiętnie na niestandardowe sposoby, potrzebujesz więcej niż `math.h`.

## Zobacz także
- [Dokumentacja `<math.h>`](https://en.cppreference.com/w/c/numeric/math)
- [Arytmetyka liczby zmiennoprzecinkowej](https://en.wikipedia.org/wiki/Floating-point_arithmetic)
- [Pułapki weryfikacji obliczeń zmiennoprzecinkowych](https://dl.acm.org/doi/10.1145/1186736.1186737)
