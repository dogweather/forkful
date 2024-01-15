---
title:                "Wycinanie podciągów"
html_title:           "Gleam: Wycinanie podciągów"
simple_title:         "Wycinanie podciągów"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/gleam/extracting-substrings.md"
---

{{< edit_this_page >}}

## Dlaczego

Ekstrakcja podciągów jest częstym wyzwaniem w programowaniu, szczególnie w przypadku manipulowania tekstami. Zrozumienie, jak to zrobić w Gleam, pozwoli Ci wykorzystać tę funkcję w swoim kodzie i ułatwi pracę z tekstami.

## Jak to zrobić

```Gleam
let text = "Witaj, świecie!"

let extracted = text.slice(7, 12)

// extracted: "świec"
```

W powyższym przykładzie używamy metody `slice(start, end)` do wydobycia podciągu "świec" z oryginalnego tekstu. Metoda ta przyjmuje dwa argumenty: początkowy i końcowy indeks, które określają, które znaki powinny znaleźć się w podciągu.

Możesz również użyć `string.get()` i `string.head()` do pobierania pojedynczego znaku lub pierwszego znaku z podciągu. Na przykład:

```Gleam
let first_letter = text.get(0)
// first_letter: "W"

let last_letter = text.slice(12).head()
//last_letter: "ń"
```

## Pełne zagłębienie

Ekstrakcja podciągów jest możliwa dzięki temu, że traktujemy tekst jako listę znaków. Metoda `slice()` pozwala nam wyciągać podciągi wykorzystując indeksy w tej liście. Dzięki temu możemy manipulować tekstami w bardziej zaawansowany sposób, co jest szczególnie przydatne przy przetwarzaniu danych.

## Zobacz też

- [Metody tekstowe w Gleam](https://gleam.run/documentation/language/strings.html#methods)
- [Gleam dokumentacja](https://gleam.run/documentation/index.html)