---
title:                "Wyszukiwanie i zastępowanie tekstu"
html_title:           "Javascript: Wyszukiwanie i zastępowanie tekstu"
simple_title:         "Wyszukiwanie i zastępowanie tekstu"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/swift/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Co i dlaczego?

Wyszukiwanie i zastępowanie tekstu to różnego rodzaju operacje na ciągach znaków. Programiści często korzystają z tych funkcji, aby zmienić dane w bardziej pożądany, użyteczny lub zrozumiały format.

## Jak to zrobić?

Swift umożliwia łatwe wyszukiwanie i zastępowanie tekstu. Skorzystajmy z przykładu. 

```Swift
var powitanie = "Cześć, Karol."
powitanie = powitanie.replacingOccurrences(of: "Karol", with: "Anna")
print(powitanie)
```

Na wyjściu zobaczysz: 

```Swift
Cześć, Anna.
```

Tutaj klasa `String` posiada funkcję `replacingOccurrences()`, która zastępuje wszystkie wystąpienia danego ciągu znaków innym ciągiem.

## Deep Dive

Operacje na ciągach znaków mają długą historię w programowaniu, od prostych operacji wykonywanych ręcznie przez programistów aż po obecne, zaawansowane metody dostępne w nowoczesnych językach programowania jak Swift.

Alternatywą dla `replacingOccurrences()` może być użycie wyrażeń regularnych (RegEx), które są bardziej elastycznymi, ale zarazem bardziej skomplikowanymi narzędziami do manipulowania tekstem.

Swift implementuje wyszukiwanie i zastępowanie tekstu poprzez protokół `StringProtocol`. Ten protokół definiuje zestaw metod, które są wspólne dla wszystkich typów, które są operacyjne jako ciągi znaków.

## Zobacz też

1. [Swift String and Characters](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html) - Podstawowe informacje o pracy ze stringami w Swift.
2. [NSRegularExpression](https://developer.apple.com/documentation/foundation/nsregularexpression) - Jak używać wyrażeń regularnych w Swift.