---
title:    "Swift: Wydobywanie podciągów"
keywords: ["Swift"]
---

{{< edit_this_page >}}

## Dlaczego?

Extraktowanie podciągów jest jedną z podstawowych umiejętności, które powinieneś opanować w języku Swift. Dzięki temu narzędziu możesz w łatwy sposób wyodrębnić konkretne części tekstu, co ułatwia pracę z danymi i znacznie przyspiesza tworzenie aplikacji.

## Jak to zrobić?

 Nie ma nic trudnego w wyodrębnianiu podciągów w języku Swift. Wystarczy użyć metody `suffix()` lub `prefix()`, w zależności od tego, czy chcesz uzyskać fragment tekstu od początku czy końca. Możesz również podać liczbę określającą długość podciągu, lub odwołać się do konkretnego indeksu.

```Swift
let exampleString = "Witaj w świecie Swift"
let prefix = exampleString.prefix(5) // output: Witaj 
let suffix = exampleString.suffix(5) // output: Swift
let index = exampleString[exampleString.index(exampleString.startIndex, offsetBy: 9)...] // output: świecie Swift
```

## Głębszy zanurzenie

Warto także zapoznać się z metodami `substring()` oraz `drop()` i `dropLast()`, które pozwalają na jeszcze większą elastyczność w wyodrębnianiu podciągów. `substring()` umożliwia określenie zakresu, z którego chcesz uzyskać podciąg, natomiast `drop()` i `dropLast()` pozwalają na usunięcie wybranych części tekstu. Warto także pamiętać o różnicy między typami `Substring` i `String` - pierwszy to "wycinek" tekstu, który odnosi się do oryginalnego łańcucha znakowego, a drugi to już kompletny tekst.

## Zobacz również

Jeśli chcesz poznać więcej metod i opcji związanych z wyodrębnianiem podciągów w języku Swift, polecam zapoznać się z oficjalnym dokumentacją oraz poniższymi linkami:

- [Oficjalna dokumentacja języka Swift](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html)
- [Poradnik na witrynie Ray Wenderlich](https://www.raywenderlich.com/166353/pattern-matching-in-swift-4-for-beginners)
- [Przykładowe zastosowanie w playgroundzie na GitHubie](https://gist.github.com/rockname/4377f04d96dfd9f8f82edb41553db0c0)