---
title:                "Swift: Wyszukiwanie i zamiana tekstu"
simple_title:         "Wyszukiwanie i zamiana tekstu"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/swift/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

# Dlaczego warto korzystać z wyszukiwania i zamiany tekstu w programowaniu Swift?

Istnieje wiele sytuacji, w których konieczne jest przeprowadzenie zmian w tekście w kodzie programu. Wyszukiwanie i zamiana tekstu w języku Swift może być przydatnym narzędziem w takich przypadkach. Pozwala to na szybką i skuteczną edycję tekstu bez konieczności ręcznego wprowadzania zmian na każdej linii kodu.

## Jak to zrobić?

Wyszukiwanie i zamiana tekstu w języku Swift może być przeprowadzone za pomocą funkcji `replacingOccurrences(of:with:)`. Oto przykład użycia:

```Swift
let text = "Programowanie w Swift jest super!"
let newText = text.replacingOccurrences(of: "super", with: "fantastyczne")
print(newText)
```

Wynikiem wyświetlonym na konsoli będzie "Programowanie w Swift jest fantastyczne!". Funkcja ta wymaga podania dwóch argumentów: tekstu, który chcemy zamienić oraz tekstu, na który chcemy go zamienić.

## Głębszy wywiad

Funkcja `replacingOccurrences(of:with:)` jest dostępna w wielu typach stringów w języku Swift, takich jak `String`, `NSString` oraz `NSStringLiteralType`. Dodatkowo, istnieją także inne funkcje pozwalające na wykonanie bardziej zaawansowanych operacji wyszukiwania i zamiany tekstu. Na przykład, funkcja `replacingOccurrences(of:with:options:range:)` pozwala na określenie zakresu, w którym ma zostać wykonana zamiana tekstu.

Warto również wspomnieć o klasie `NSRegularExpression`, która pozwala na wyrażenia regularne w celu dopasowania bardziej złożonego tekstu do wyszukiwania.

# Zobacz również
- [Dokumentacja Apple - String](https://developer.apple.com/documentation/swift/string)
- [Dokumentacja Apple - NSString](https://developer.apple.com/documentation/foundation/nsstring)
- [Dokumentacja Apple - NSRegularExpression](https://developer.apple.com/documentation/foundation/nsregularexpression)