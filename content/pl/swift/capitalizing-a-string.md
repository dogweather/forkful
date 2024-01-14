---
title:                "Swift: Zmiana wielkości liter w ciągu znaków"
simple_title:         "Zmiana wielkości liter w ciągu znaków"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/swift/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Dlaczego kapitalizować string?

Kapitalizowanie stringów jest częstym zadaniem w programowaniu, które polega na zamianie pierwszej litery każdego słowa w stringu na wielką. Jest to przydatne w przypadku, gdy chcemy wyświetlić tekst w sposób bardziej estetyczny lub gdy wymagane jest, aby wielkość liter była zachowana w danym kontekście.

## Jak to zrobić?

Możemy to łatwo osiągnąć w języku Swift, korzystając z metody `capitalized` na obiekcie typu `String`. Przykładowy kod wyglądałby następująco:

```Swift
let text = "to jest przykładowy string"
let capitalizedText = text.capitalized
print(capitalizedText) // "To Jest Przykładowy String"
```

## Zagłębienie

W przypadku, gdy chcemy kapitalizować tylko pierwszą literę stringa, a pozostałe zostawić bez zmian, możemy użyć metody `capitalizingFirstLetter`, podobnie jak w poniższym przykładzie:

```Swift
let text = "to jest przykładowy string"
let firstLetterCapitalized = text.capitalizingFirstLetter
print(firstLetterCapitalized) // "To jest przykładowy string"
```

Możemy również dostosować zachowanie kapitalizowania za pomocą opcji przekazanych do parametru `options` w metodzie `capitalized` lub `capitalizingFirstLetter`. Więcej informacji na ten temat można znaleźć w dokumentacji języka Swift.

## Zobacz również

- Dokumentacja języka Swift: [https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html#ID306](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html#ID306) 
- Blog o programowaniu w języku Swift: [https://swiftbysundell.com/](https://swiftbysundell.com/) 
- Poradnik na temat manipulacji stringami w języku Swift: [https://learnappmaking.com/string-swift-programming-how-to/](https://learnappmaking.com/string-swift-programming-how-to/)