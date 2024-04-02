---
date: 2024-01-20 17:37:54.074998-07:00
description: "Zmiana napis\xF3w na ma\u0142e litery oznacza przekszta\u0142cenie wszystkich\
  \ liter w ci\u0105gu znak\xF3w na ich ma\u0142e odpowiedniki. Robimy to dla ujednolicenia\
  \ danych,\u2026"
lastmod: '2024-03-13T22:44:35.699887-06:00'
model: gpt-4-1106-preview
summary: "Zmiana napis\xF3w na ma\u0142e litery oznacza przekszta\u0142cenie wszystkich\
  \ liter w ci\u0105gu znak\xF3w na ich ma\u0142e odpowiedniki. Robimy to dla ujednolicenia\
  \ danych,\u2026"
title: "Konwersja ci\u0105gu znak\xF3w na ma\u0142e litery"
weight: 4
---

## What & Why? (Co i Dlaczego?)
Zmiana napisów na małe litery oznacza przekształcenie wszystkich liter w ciągu znaków na ich małe odpowiedniki. Robimy to dla ujednolicenia danych, łatwiejszego porównywania stringów, czy też przygotowania tekstu do wyszukiwań czy sortowań niezależnych od wielkości liter.

## How to (Jak to zrobić):
Chcąc zmienić napis na małe litery w C++, używamy standardowej biblioteki `<algorithm>` i funkcji `std::transform` wraz z `::tolower`.

```C++
#include <iostream>
#include <string>
#include <algorithm>
#include <cctype>

int main() {
    std::string text = "Witaj, Świecie!";
    std::transform(text.begin(), text.end(), text.begin(),
                   [](unsigned char c){ return std::tolower(c); });

    std::cout << text << std::endl;
    // Output: witaj, świecie!
    return 0;
}
```

## Deep Dive (Głębsze spojrzenie)
Konwersja na małe litery jest prostym, ale ważnym narzędziem w programowaniu. Kiedyś musieliśmy pisać własne pętle by to zrobić. Teraz `std::transform` i `::tolower` robią wszystko za nas, obsługując nawet różne lokalizacje.

Inne metody to używanie metod `std::for_each` lub pisania własnej pętli for. Warto też wspomnieć o bibliotece Boost, która oferuje bogatszą funkcjonalność. Implementation może się różnić w zależności od charsetu (np. ASCII vs. Unicode) i lokalizacji.

Dużym wyzwaniem w C++ jest wielojęzykowość i obsługa różnych systemów kodowania, takich jak UTF-8. Biblioteki takie jak ICU mogą być pomocne w przypadku gdy standardowe metody zawodzą, szczególnie gdy pracujemy z nie-angielskimi zestawami znaków.

## See Also (Zobacz również)
- Dokumentacja C++ `std::transform`: http://www.cplusplus.com/reference/algorithm/transform/
- Dokumentacja C++ `std::tolower`: http://www.cplusplus.com/reference/cctype/tolower/
- Unicode Technical Report #21: https://unicode.org/reports/tr21/tr21-5.html
- International Components for Unicode (ICU) Library: http://site.icu-project.org/
