---
title:                "Znajdowanie długości ciągu znaków"
html_title:           "Gleam: Znajdowanie długości ciągu znaków"
simple_title:         "Znajdowanie długości ciągu znaków"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/gleam/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Dlaczego

Któż nie był kiedyś zaintrygowany tym, jak dokładnie dane są przechowywane w naszych programach? W tym artykule dowiesz się, jak wykorzystać język programowania Gleam, aby obliczyć długość ciągu znaków.

## Jak to zrobić

Gleam jest językiem zaprojektowanym z myślą o wydajności, wyraźnym kodzie i łatwej integracji z innymi językami. Jeśli jesteś już zaznajomiony z podstawową składnią Gleam, możesz przejść do głównego tematu - jak obliczyć długość ciągu znaków.

```Gleam
let string = "Witaj, świecie!"
let length = string |> String.length

#[Output]
length: Int
```

W tym przykładzie tworzymy zmienną `string` i przypisujemy jej wartość "Witaj, świecie!". Następnie wykorzystujemy funkcję `String.length` w celu obliczenia długości ciągu. Wynik zostaje przypisany do zmiennej `length`, która jest typu `Int`.

Teraz moglibyśmy wypisać wartość zmiennej `length` w konsoli, aby sprawdzić czy działa poprawnie. Jednak w rzeczywistych projektach często zachodzi potrzeba wykorzystania długości ciągu w inny sposób, na przykład w warunkowych instrukcjach lub pętlach. Dzięki znajomości tego sposobu obliczania długości ciągu, możesz wykorzystać ją w swoich projektach Gleam.

## Deep Dive

Dla tych, którzy chcą lepiej zrozumieć, jak dokładnie działa funkcja `String.length`, przyjrzyjmy się jej implementacji. Wewnątrz języka Gleam funkcja ta jest zdefiniowana jako:

```Gleam
fn length(string) {
  length = string -> Char.count()
  0..string |> Enum.each(_) { length += 1 }
  length
}
```

W tej implementacji najpierw definiujemy zmienną `length`, która jest przypisana do wywołania funkcji `Char.count()` na danych wejściowych `string`. Następnie wykorzystujemy pętlę `each` do iteracji po każdym elemencie ciągu i dodania do zmiennej `length` wartości 1. W ten sposób otrzymujemy prawidłową długość ciągu znaków.

## Zobacz również

- [Dokumentacja języka Gleam](https://gleam.run)
- [Przykłady wykorzystania języka Gleam](https://github.com/gleam-lang/gleam/tree/master/examples)
- [Kurs programowania w języku Gleam](https://rogertorres.gitlab.io/gleam-book/)