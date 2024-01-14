---
title:                "Swift: Konwersja ciągu znaków na małe litery"
simple_title:         "Konwersja ciągu znaków na małe litery"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/swift/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Dlaczego przekształcać string na małe litery?

Często w programowaniu spotykamy się z potrzebą manipulacji tekstami. Jedną z takich czynności może być zmiana wszystkich liter w tekście na małe. W tym artykule dowiesz się, dlaczego jest to przydatne oraz jak to zrobić w języku Swift.

## Jak to zrobić?

Przekształcenie wszystkich liter w tekście na małe w języku Swift jest bardzo łatwe i wymaga użycia jednej metody. Przykładowy kod wygląda następująco:

```Swift
let text = "Cześć, to jest PRZYKŁADOWY TEKST"
let lowercasedText = text.lowercased()
print(lowercasedText)
```
Wynik:

 `cześć, to jest przykładowy tekst`

W powyższym przykładzie, deklarujemy zmienną `text` zawierającą nasz tekst do przekształcenia. Następnie, używając metody `lowercased()`, tworzymy nową zmienną `lowercasedText` zawierającą przekształcony tekst. Na końcu, wypisujemy uzyskany wynik za pomocą funkcji `print()`.

Metryda `lowercased()` może być również wywołana na całym Stringu, dzięki czemu nie musimy tworzyć dodatkowej zmiennej. Przykład:

```Swift
let text = "Cześć, to jest PRZYKŁADOWY TEKST"
print(text.lowercased())
```
Wynik:

`cześć, to jest przykładowy tekst`

## Deep Dive

Przekształcanie stringów na małe litery jest możliwe dzięki użyciu funkcji `lowercased()`, która jest dostępna dla typu `String` w języku Swift. Metryda ta wykorzystuje mechanizm zwany Unicode, który odpowiada za dokonywanie przekształceń znaków. Dzięki temu, nawet dla tekstów z literami z alfabetu nie-łacińskiego (np. japońskiego czy chińskiego), funkcja `lowercased()` zadziała poprawnie i przekształci wszystkie litery na małe.

## Zobacz również

Przekształcając string na małe litery, możemy znacznie ułatwić sobie pracę z tekstami w aplikacjach pisanych w języku Swift. Poniżej znajdują się linki do innych artykułów, które mogą Ci się przydać:

- [Oficjalna dokumentacja języka Swift](https://developer.apple.com/documentation/swift)
- [Jak używać stringów w języku Swift](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html) (angielski)