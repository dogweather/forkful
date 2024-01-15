---
title:                "Porównywanie dwóch dat"
html_title:           "Gleam: Porównywanie dwóch dat"
simple_title:         "Porównywanie dwóch dat"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/gleam/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Dlaczego

Porównywanie dat jest nieodłączną częścią wielu programów i aplikacji, szczególnie tych związanych z czasem lub planowaniem. Dzięki temu możliwe jest ustalenie, która z dat jest wcześniejsza lub późniejsza, co może być istotne w różnych sytuacjach. W tym artykule dowiesz się, jak w łatwy sposób porównać dwie daty za pomocą języka programowania Gleam.

## Jak to zrobić

Aby porównać dwie daty w Gleam, wykorzystaj funkcję `Date.compare` oraz operator porównania `>`, `<` lub `==` w zależności od potrzeby. Oto przykładowy kod porównujący daty w formacie `YYYY-MM-DD`:

```Gleam
let date1 = Date.from_string("2021-01-01")
let date2 = Date.from_string("2021-02-15")

if date1 > date2 {
  // date1 jest późniejsza niż date2
} else if date1 < date2 {
  // date1 jest wcześniejsza niż date2
} else {
  // daty są identyczne
}
```

W takim przypadku, porównanie daty1 z datą2 daje wynik `true` dla operacji porównania `>`, `false` dla `<` i `false` dla `==`. Możesz także wykorzystać tę samą metodę dla dat w innych formatach, jak np. `MM/DD/YYYY` czy `DD/MM/YY`.

## Głębsze zanurzenie

W języku Gleam, daty są reprezentowane za pomocą typu `Date`, który przechowuje wartości roczne, miesięczne i dzienne. Funkcja `Date.compare` porównuje te wartości dla dwóch dat. Dla przykładu, gdy porównujemy daty w formacie `YYYY-MM-DD`, funkcja `compare` będzie porównywać najpierw rok, a następnie miesiąc i dzień.

Warto także wiedzieć, że funkcja `Date.from_string` może przyjmować różne formaty dat, jednakże należy upewnić się, że format odpowiada temu używanemu w porównaniu za pomocą funkcji `Date.compare`.

## Zobacz także

- [Dokumentacja języka Gleam na temat typu Date](https://gleam.run/documentation/standard-library/date/)
- [Porównywanie dat w języku programowania JavaScript](https://www.w3schools.com/js/js_dates.asp)