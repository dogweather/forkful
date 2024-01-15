---
title:                "Konwertowanie ciągu znaków na małe litery"
html_title:           "Swift: Konwertowanie ciągu znaków na małe litery"
simple_title:         "Konwertowanie ciągu znaków na małe litery"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/swift/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Dlaczego

Jeśli pracujesz z tekstem w języku Swift, możesz chcieć przekonwertować wprowadzone dane na małe litery. To ważna część przetwarzania tekstu, ponieważ upraszcza porównywanie i analizowanie danych. W tym artykule pokażemy Ci, jak przekonwertować string na małe litery w Swift.

## Jak To Zrobić

Aby przekonwertować string na małe litery w Swift, użyjemy metody ".lowercased()". Oto prosty przykład kodu w języku Swift:

```Swift
let text = "HEllo WoRLd"
let lowercasedText = text.lowercased()
print(lowercasedText)
```

Ten kod spowoduje wydrukowanie "hello world" w konsoli.

Możesz również użyć metody ".lowercased()" do konwersji pojedynczego znaku na małą literę:

```Swift
let letter = "A"
let lowercaseLetter = letter.lowercased()
print(lowercaseLetter)
```

Ten kod spowoduje wydrukowanie "a" w konsoli. Możesz także użyć tej metody na całych wyrażeniach:

```Swift
let sentence = "I EnJoy ProgRaMmING"
let lowercasedSentence = sentence.lowercased()
print(lowercasedSentence)
```

Ten kod spowoduje wydrukowanie "i enjoy programming" w konsoli.

## Głębsza Analiza

Podczas wykorzystywania metody ".lowercased()" należy pamiętać o regionalnych ustawieniach urządzenia użytkownika. Dla przykładu, w niektórych językach, np. w języku łacińskim, wielkie litery są tylko pewnymi wariantami małych liter. W takich przypadkach, metoda ".lowercased()" nie zmieni wielkich na małe litery, ponieważ są one niezmienne w tym kontekście.

W języku Swift istnieje również możliwość przekonwertowania tekstu na wielkie litery, korzystając z metody ".uppercased()". Możesz również skorzystać z metody ".capitalized()", która przekonwertuje pierwszą literę każdego wyrazu na wielką.

## Zobacz Również

Jeśli chcesz dowiedzieć się więcej o pracy z tekstem w języku Swift, zerknij na te linki:

- [Oficjalna dokumentacja Swifta - String](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html)
- [Convert String to Lowercase in Swift](https://www.hackingwithswift.com/example-code/strings/how-to-convert-a-string-to-lowercase-in-swift)
- [Working with Strings in Swift](https://www.swiftbysundell.com/articles/working-with-strings-in-swift/)

Dziękujemy za przeczytanie tego artykułu i miej się dobrze!