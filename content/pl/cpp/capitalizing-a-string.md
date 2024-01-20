---
title:                "Zamiana małych liter na wielkie w ciągu znaków"
html_title:           "C++: Zamiana małych liter na wielkie w ciągu znaków"
simple_title:         "Zamiana małych liter na wielkie w ciągu znaków"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/cpp/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Co i dlaczego?

Zamiana małych liter na wielkie, czyli tzw. 'capitalization', to proste przekształcenie tekstu, używane w programowaniu do poprawy czytelności kodu. Programiści robią to na przykład po to, żeby odróżniać stałe od zmiennych. 

## Jak to zrobić:

```C++
#include <algorithm>
#include <cctype>

std::string wejsciowyTekst = "witaj, świecie";
std::transform(wejsciowyTekst.begin(), wejsciowyTekst.end(), wejsciowyTekst.begin(), ::toupper);
```
Przykładowe wyjście:

```C++
"WITAJ, ŚWIECIE"
```

## Głębsze zanurzenie:

Historia funkcji `toupper` sięga pierwszych dni języka C, jako jednej z wbudowanych funkcji obsługujących znaki. Alternatywą dla `transform` i `toupper` byłoby manualne przekształcenie każdego znaku w naszym łańcuchu, ale jest to mniej wydajne. Implementacja `toupper` jest specyficzna dla platformy, ale zazwyczaj polega na dodaniu stałej wartości do kodowania ASCII danego znaku. 

## Zobacz też:

Więcej informacji o transformacji i funkcjach znakowych w C++ można znaleźć tutaj: 
[link do www.cplusplus.com](https://www.cplusplus.com/reference/string/string/transform/)