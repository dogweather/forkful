---
title:                "Swift: Używanie wyrażeń regularnych"
programming_language: "Swift"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/swift/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Dlaczego

Często przy tworzeniu aplikacji mobilnych lub stron internetowych, musimy przetwarzać lub wyszukiwać specyficzne ciągi znaków. W takich sytuacjach przydatne mogą okazać się wyrażenia regularne, które pozwalają nam znacznie łatwiej i szybciej manipulować tekstem. W tym artykule dowiesz się, co to są wyrażenia regularne i jak możesz zacząć z nich korzystać w swoim kodzie w języku Swift.

## Jak to zrobić

Wyrażenia regularne są wyrażeniami, które pozwalają na wyszukiwanie i manipulowanie tekstem według określonego wzorca. W języku Swift, wykorzystujemy do tego dedykowaną klasę NSRegularExpression. Aby zacząć korzystać z wyrażeń regularnych, musimy najpierw utworzyć instancję tej klasy:

```Swift
let pattern = "[a-z]+"
// Tworzymy wyrażenie regularne dla wyszukiwania dowolnego ciągu małych liter
let regex = try NSRegularExpression(pattern: pattern, options: [])
```

Następnie możemy użyć tej instancji do przetwarzania tekstu, na przykład:

```Swift
let text = "Ten tekst będzie przetworzony."
let matches = regex.matches(in: text, options: [], range: NSRange(text.startIndex..., in: text))
// Wykorzystujemy metodę matches do znalezienia wszystkich dopasowań wyrażenia regularnego w tekście
```

W ten sposób możemy uzyskać listę dopasowań, które w podanym tekście spełniają nasz wzorzec. Aby dowiedzieć się więcej na temat możliwości klasy NSRegularExpression, możesz zajrzeć do dokumentacji Apple: [NSRegularExpression - Apple Developer Documentation](https://developer.apple.com/documentation/foundation/nsregularexpression)

## Głębsza analiza

Język Swift udostępnia wiele przydatnych funkcji dla pracy z wyrażeniami regularnymi. Możemy na przykład sprawdzać czy dany tekst zawiera podany wzorzec, użyć wyrażenia regularnego jako wzorca w innych wyrażeniach, lub dodawać zmienne do naszego wzorca za pomocą operatora `\` (np. `\w` oznacza dowolny znak alfanumeryczny).

Aby dowiedzieć się więcej na temat możliwości i składni wyrażeń regularnych w Swift, zapraszam do zapoznania się z dokumentacją języka: [Regular Expressions - The Swift Programming Language (Swift 5.5.1)](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html#ID293)

## Zobacz także

* [NSRegularExpression - Apple Developer Documentation](https://developer.apple.com/documentation/foundation/nsregularexpression)
* [Regular Expressions - The Swift Programming Language (Swift 5.5.1)](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html#ID293)
* [Regular Expressions Cheat Sheet](https://www.rexegg.com/regex-quickstart.html)
* [Regex Tester - Regular Expression Tester](https://regexr.com/)