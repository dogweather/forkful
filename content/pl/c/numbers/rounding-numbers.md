---
aliases:
- /pl/c/rounding-numbers/
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 18:07:45.330322-07:00
description: "Zaokr\u0105glanie liczb to proces dostosowywania cyfr liczby w celu\
  \ zmniejszenia jej precyzji zgodnie z pewnymi zasadami, albo w kierunku najbli\u017C\
  szej liczby\u2026"
lastmod: 2024-02-18 23:08:50.074390
model: gpt-4-0125-preview
summary: "Zaokr\u0105glanie liczb to proces dostosowywania cyfr liczby w celu zmniejszenia\
  \ jej precyzji zgodnie z pewnymi zasadami, albo w kierunku najbli\u017Cszej liczby\u2026"
title: "Zaokr\u0105glanie liczb"
---

{{< edit_this_page >}}

## Co i dlaczego?

Zaokrąglanie liczb to proces dostosowywania cyfr liczby w celu zmniejszenia jej precyzji zgodnie z pewnymi zasadami, albo w kierunku najbliższej liczby całkowitej, albo określonej liczby miejsc dziesiętnych. Programiści robią to z różnych powodów, począwszy od ograniczenia potrzebnej ilości pamięci, poprzez upraszczanie wyników dla użytkownika, aż po zapewnienie dokładności operacji matematycznych, które są wrażliwe na bardzo małe zmiany.

## Jak to zrobić:

Zaokrąglanie liczb w C można osiągnąć za pomocą różnych funkcji, ale najczęściej stosowane są funkcje `floor()`, `ceil()`, i `round()`. Te funkcje są częścią standardowej biblioteki matematycznej, więc musisz dołączyć `math.h` do swojego programu.

```c
#include <stdio.h>
#include <math.h>

int main() {
    double num = 9.527;

    // Używanie floor() do zaokrąglenia w dół
    double floorResult = floor(num);
    printf("floor(9.527) = %.0f\n", floorResult);

    // Używanie ceil() do zaokrąglenia w górę
    double ceilResult = ceil(num);
    printf("ceil(9.527) = %.0f\n", ceilResult);

    // Używanie round() do zaokrąglenia do najbliższej liczby całkowitej
    double roundResult = round(num);
    printf("round(9.527) = %.0f\n", roundResult);

    // Zaokrąglanie do określonej liczby miejsc dziesiętnych wymaga mnożenia i dzielenia
    double twoDecimalPlaces = round(num * 100) / 100;
    printf("Zaokrąglanie do dwóch miejsc po przecinku: %.2f\n", twoDecimalPlaces);

    return 0;
}
```

Wynik:
```
floor(9.527) = 9
ceil(9.527) = 10
round(9.527) = 10
Zaokrąglanie do dwóch miejsc po przecinku: 9.53
```

## Pogłębiona analiza

Zaokrąglanie liczb ma głębokie korzenie historyczne w matematyce i obliczeniach, będąc integralną częścią zarówno teoretycznych, jak i stosowanych aspektów. W C, chociaż `floor()`, `ceil()`, i `round()` oferują podstawową funkcjonalność, istota zaokrąglania liczb zmiennoprzecinkowych do liczb całkowitych lub określonych miejsc dziesiętnych jest bardziej złożona z powodu binarnej reprezentacji liczb zmiennoprzecinkowych. Ta reprezentacja może prowadzić do nieoczekiwanych wyników, ze względu na sposób, w jaki obsługiwane są liczby, które nie mogą być dokładnie przedstawione w systemie binarnym (takie jak 0.1).

Te funkcje są częścią biblioteki standardowej C, zdefiniowanej w `<math.h>`. Przy zaokrąglaniu liczb, szczególnie dla obliczeń finansowych lub precyzyjnych obliczeń inżynierskich, należy wziąć pod uwagę implikacje stosowania binarnych liczb zmiennoprzecinkowych. Alternatywy dla wbudowanych funkcji C dla dokładnego lub specyficznego zaokrąglania dziesiętnego mogą obejmować implementację własnych funkcji zaokrąglających lub użycie bibliotek zaprojektowanych do arytmetyki o dowolnej precyzji, takich jak GMP lub MPFR, chociaż wprowadzają one dodatkową złożoność i zależności.

W praktyce wybór odpowiedniego podejścia do zaokrąglania w C wiąże się z równoważeniem potrzeb precyzji, wydajności i praktyczności, z głębokim zrozumieniem specyficznych wymagań domenowych aplikacji, którą się rozwija.
