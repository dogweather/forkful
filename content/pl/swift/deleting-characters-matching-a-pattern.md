---
title:                "Usuwanie znaków pasujących do wzorca"
html_title:           "Swift: Usuwanie znaków pasujących do wzorca"
simple_title:         "Usuwanie znaków pasujących do wzorca"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/swift/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Dlaczego

Czyszczenie tekstu jest ważnym elementem wielu projektów programistycznych. Znajdowanie i usuwanie znaków spełniających określony wzór jest szczególnie przydatne przy analizie danych tekstowych lub implementacji wyrażeń regularnych.

## Jak to zrobić

Jeśli chcesz usunąć znaki pasujące do wzorca w języku Swift, możesz użyć metody `replacingOccurrences(of:with:options:)` na typie `String`. Poniżej znajduje się przykład kodu, który usuwa wszystkie wystąpienia litery "a" z podanego tekstu:

```Swift
let text = "abcabcabc"
let newText = text.replacingOccurrences(of: "a", with: "", options: .literal, range: nil)
print(newText) // bcbcbc
```

Jest wiele opcji do wyboru, takich jak określenie zakresu, w którym ma zostać wykonana zmiana, czy też wykorzystanie wyrażeń regularnych. Więcej informacji znajdziesz w dokumentacji języka Swift.

## Głębszy wgląd

Usunięcie znaków pasujących do wzorca może być również wykonane przy użyciu wyrażeń regularnych. W języku Swift można skorzystać z biblioteki `NSRegularExpression` i jej metody `replaceMatches(in:options:range:withTemplate:)`. Przykładowe użycie wyrażeń regularnych do zastępowania wszystkich cyfr w tekście zostanie przedstawione poniżej:

```Swift
let text = "A1B2C3"
let regex = try! NSRegularExpression(pattern: "\\d", options: [])
let newText = regex.stringByReplacingMatches(in: text, options: [], range: NSMakeRange(0, text.count), withTemplate: "")
print(newText) // ABC
```

Język Swift oferuje także przydatne metody do walidacji i wykrywania znaków spełniających określone wzorce. Warto zapoznać się z dokumentacją w celu lepszego zrozumienia możliwości tego języka.

## Zobacz także

Jeśli chcesz dowiedzieć się więcej o manipulacji tekstem w języku Swift, polecamy zapoznanie się z poniższymi linkami:

- [Dokumentacja języka Swift](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html)
- [Przewodnik po wyrażeniach regularnych w języku Swift](https://www.swiftbysundell.com/basics/regular-expressions/)
- [Przykłady użycia wyrażeń regularnych w języku Swift](https://code.tutsplus.com/tutorials/swift-3-part-4-intermediate-regular-expressions--cms-27120)