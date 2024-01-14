---
title:                "Swift: Znajdowanie długości ciągu znaków"
simple_title:         "Znajdowanie długości ciągu znaków"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/swift/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Dlaczego

Często w programowaniu musimy operować na tekście - może to być użytkowa treść, użytkownikowe dane lub elementy interfejsu użytkownika. Jedną z podstawowych operacji na tekście jest obliczanie jego długości. W tym artykule dowiesz się, dlaczego jest to ważne i jak to zrobić w języku Swift.

## Jak to zrobić

W języku Swift długość tekstu możemy obliczyć przy użyciu metody `count` na typie `String`. Na przykład:

```Swift
let tekst = "Witaj, świecie!"
print(tekst.count)
```

Wyższy kod wyświetli wartość `14` w konsoli. Pamiętaj, że funkcja `count` zwraca ilość **znaków**, a nie **liter** - więc polskie litery będą liczone jako jedna wartość.

## Pogłębione informacje

Podczas obliczania długości tekstu w języku Swift, należy pamiętać, że puste znaki są również liczone jako jeden znak. Niektóre specjalne znaki, takie jak emoji, mogą być liczone jako kilka znaków. Dodatkowo, również uwzględniana jest informacja o kodowaniu tekstu - na przykład w Unicode znaki poza standardowym zakresem mają większą długość. Istnieje również możliwość obliczania długości tylko określonej części tekstu za pomocą metody `prefix` lub `suffix`.

## Zobacz również

- [Dokumentacja Swift - String](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html)
- [Podstawy Swift: Tekst](https://codeburst.io/suzy-potatohead-101-how-to-use-swift-strings-93e7ec3e6755)
- [Podstawy Swift: Teksty i łańcuchy (wideo po polsku)](https://www.youtube.com/watch?v=NoYmxHIskQg)