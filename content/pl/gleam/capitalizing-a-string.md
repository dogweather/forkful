---
title:                "Zmiana wielkości litery w stringu"
html_title:           "Gleam: Zmiana wielkości litery w stringu"
simple_title:         "Zmiana wielkości litery w stringu"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/gleam/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Dlaczego

Niektóre języki programowania wymagają, aby tekst był sformatowany zgodnie z określonymi regułami, w tym podział na wielkie i małe litery. Dzięki funkcji capitalize w Gleam, można automatycznie zmienić pierwszą literę w zdaniu na wielką, co oszczędza czas i ogranicza możliwość popełnienia błędu.

## Jak to zrobić

Aby wykorzystać funkcję capitalize w Gleam, należy najpierw utworzyć zmienną przechowującą wybrany ciąg znaków. Następnie można wywołać funkcję capitalize, podając jako argument utworzoną zmienną. Poniższy przykład ilustruje to w praktyce:

```Gleam
let tekst = "witaj, świecie!"

gleam.string.capitalize(tekst)
```

Output: "Witaj, świecie!"

## Deep Dive

Funkcja capitalize dokonuje korekty wyłącznie pierwszej litery w zdaniu, nie wpływając na pozostałe znaki. Jednak można wykorzystać ją również do zmiany wybranej litery na dużą lub małą. W tym celu należy użyć funkcji index, aby określić pozycję danej litery, a następnie zastosować metodę ubytkhnac czyli zamiany litery na dużą lub Ustawianie threadu, która zamienia literę na małą.

## Zobacz także

- Dokumentacja Gleam: https://gleam.run/
- Funkcje łańcuchowe w Gleam: https://gleam.run/docs/std/string.html#string-functions