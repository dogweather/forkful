---
title:                "Używanie wyrażeń regularnych"
html_title:           "Swift: Używanie wyrażeń regularnych"
simple_title:         "Używanie wyrażeń regularnych"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/swift/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Dlaczego
Wynikiem użycia wyrażeń regularnych jest oszczędność czasu i precyzja filtrowania i wyszukiwania tekstu. Jest to szczególnie przydatne w przypadku dużej ilości danych lub złożonych wzorców do znalezienia.

## Jak to zrobić
Wyrażenia regularne w Swift umożliwiają wyszukiwanie i manipulowanie tekstem na podstawie określonych wzorców. Najpierw musimy zaimportować bibliotekę `Foundation`, wykorzystywaną w wielu frameworkach Apple. Następnie używamy metody `range(of:)`, aby znaleźć dopasowanie do naszego wzorca w łańcuchu tekstowym. Na przykład:

```
import Foundation

// znajdź wystąpienie wyrażenia regularnego w tekście
let text = "Kocham programowanie w Swift!"
let pattern = "programowanie"
if let range = text.range(of: pattern) {
    print("Znaleziono dopasowanie na pozycjach \(range.lowerBound) - \(range.upperBound)")
} else {
    print("Nie znaleziono dopasowania")
}

// aktualizuj tekst na podstawie wyrażenia regularnego
let newText = text.replacingOccurrences(of: pattern, with: "kodowanie")
print(newText) // "Kocham kodowanie w Swift!"
```

## Głębsza analiza
Wyrażenia regularne pozwalają na użycie specjalnych znaków i symboli do stworzenia bardziej złożonych wzorców. Na przykład:

- `[]` - znaki wyboru, np. `[a-z]` oznacza wszystkie małe litery
- `*` - symbol wielokrotnej ilości, np. `[a-z]*` dopasowuje do dowolnej liczby małych liter
- `+` - symbol jednokrotnej lub większej ilości, np. `[0-9]+` dopasowuje do dowolnej liczby cyfr
- `^` - symbol początku linii, np. `^Swift` dopasowuje do linii zaczynającej się od słowa "Swift"
- `$` - symbol końca linii, np. `value$` dopasowuje do linii kończącej się na słowie "value"

Istnieje wiele innych znaków specjalnych i możliwości manipulacji tekstem za pomocą wyrażeń regularnych, dlatego warto zagłębić się w ten temat, aby wykorzystać je w pełni.

## Zobacz także
- [Podstawy wyrażeń regularnych w Swift](https://medium.com/@jagdeep1/wymaganie-8-podstawy-wyra%C5%BCe%C5%84-regularnych-d6be6a766474)
- [Oficjalna dokumentacja Swift - Wyrażenia regularne](https://developer.apple.com/documentation/foundation/nsregularexpression)
- [Kurs wideo o wyrażeniach regularnych w Swift](https://www.youtube.com/watch?v=rXl9EvoUlIU)