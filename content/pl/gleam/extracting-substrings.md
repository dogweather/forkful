---
title:                "Gleam: Wycinanie podciągów"
simple_title:         "Wycinanie podciągów"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/gleam/extracting-substrings.md"
---

{{< edit_this_page >}}

## Dlaczego
Często w procesie pisania kodu napotykamy sytuacje, w których musimy wyodrębnić pewien fragment tekstu z większego ciągu znaków. W takich przypadkach wykorzystanie funkcji do wycinania podciągów może być niezwykle pomocne.

## Jak to zrobić
Aby wyodrębnić substring z tekstu w języku Gleam, należy skorzystać z funkcji `String.slice()`, podając jej jako argument indeks początkowy oraz końcowy wycięcia. Przykładowy kod wyglądałby następująco:

```Gleam
let text = "Witaj, Gleam!"
let substring = String.slice(text, 7, 12) // wynik: "Gleam"
```

Możemy również ustawić indeks końcowy na `String.length(text)` aby pobrać podciąg od wybranego indeksu do końca tekstu. W przypadku gdy nie znamy dokładnych długości tekstu, możemy wykorzystać funkcję `String.split()` do podzielenia tekstu na wybrane znaki specjalne lub słowa.

## Deep Dive
Funkcja `String.slice()` wykorzystuje indeksowanie znaków Unicode, co oznacza, że możemy bez problemu wyodrębnić podciągi z tekstów w różnych językach. Ponadto, argumenty funkcji mogą przyjmować także wartości ujemne, co pozwala nam wybierać znaki od końca tekstu.

Jednym z zastosowań funkcji do wycinania podciągów może być np. sprawdzanie czy dany fragment tekstu istnieje w większym tekście.

## Zobacz także
- Dokumentacja funkcji `String.slice()`: https://gleam.run/core/string.html#slice
- Wyciąganie podciągów z wykorzystaniem pattern matchingu: https://blog.carbonfive.com/2019/03/01/pattern-matching-in-gleam/