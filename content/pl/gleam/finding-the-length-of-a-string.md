---
title:                "Gleam: Znajdowanie długości ciągu znaków"
programming_language: "Gleam"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/gleam/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Dlaczego

Pojęcie "długość napisu" jest powszechnie spotykane w programowaniu i często stanowi kluczowy element w manipulowaniu tekstem. Znalezienie długości napisu może pomóc w walidacji danych, tworzeniu pętli oraz w innych zadaniach związanych z tekstem. W tym artykule dowiesz się, dlaczego znajomość tego zagadnienia może być przydatna w codziennej pracy programisty.

## Jak znaleźć długość napisu w Gleam

Aby znaleźć długość napisu w języku Gleam, należy użyć funkcji `length` z modułu `String`. Poniżej przedstawione są trzy przykładowe przypadki korzystania z tej funkcji:

```Gleam
import String

let name = "Maria"
let length = String.length(name)
// Wynik: 5

let text = "Ten tekst zawiera 29 znaków."
let length = String.length(text)
// Wynik: 29

let empty_string = ""
let length = String.length(empty_string)
// Wynik: 0
```

Jak widać, funkcja `length` zwraca liczbę całkowitą reprezentującą długość danego napisu.

## Głębszy zanurzenie

W języku Gleam, długość napisu jest obliczana na podstawie liczby kodów Unicode, co może zaskoczyć programistów korzystających z innych języków programowania. Ważne jest też zwrócenie uwagi na specjalne znaki, takie jak emoji czy znaki diakrytyczne, które mogą wpłynąć na długość napisu.

Warto również wykorzystywać funkcję `length` w połączeniu z innymi funkcjami dostępnymi w module `String`, na przykład do wycinania podciągów o określonej długości.

## Zobacz również

- Dokumentacja języka Gleam: [String module](https://gleam.run/modules/string.html)
- Przewodnik po języku Gleam: [Manipulacja tekstem](https://gleam.run/book/tips-and-tricks/manipulating-text.html)
- Wideo tutorial: [Jak znaleźć długość napisu w Gleam](https://www.youtube.com/watch?v=2HUbXDMGPhQ)