---
title:                "Konwersja ciągu znaków na małe litery"
html_title:           "Swift: Konwersja ciągu znaków na małe litery"
simple_title:         "Konwersja ciągu znaków na małe litery"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/swift/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Co to jest i dlaczego to robimy?

Konwertowanie tekstu do małych liter (lower case) jest procesem zmieniania wszystkich liter w tekście na ich "niskie" odpowiedniki. Często wykonujemy to, aby ujednolicić nasze dane lub upewnić się, że porównanie tekstu będzie działać poprawnie.

## Jak to zrobić:

``` Swift
let text = "Hello World"
let lowerCaseText = text.lowercased()

print(lowerCaseText)

// Output: hello world
```

## Głębszy zanurzenie:

1. Kontekst historyczny: Konwersja tekstu na małe litery jest praktykowana od dawna, aby ułatwić zarządzanie tekstem i poprawić jego czytelność.

2. Alternatywy: Istnieje wiele innych sposobów na konwersję tekstu na małe litery, m.in. zmiana kodowania lub użycie bibliotek zewnętrznych.

3. Szczegóły implementacji: W języku Swift, konwersję tekstu na małe litery możemy wykonać za pomocą metody `lowercased ()` na typie `String`.

## Zobacz także:

- Dokumentacja Swift: https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html
- Porównywanie i sortowanie równoznacznych ciągów znaków: https://developer.apple.com/documentation/swift/string/1786178-equal
- Poradnik na temat zarządzania tekstem w języku Swift: https://www.appcoda.com/swift-string/