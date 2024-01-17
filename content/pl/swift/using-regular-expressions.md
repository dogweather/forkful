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

## Co i dlaczego?
Używanie wyrażeń regularnych to technika, która umożliwia programistom przeszukiwanie i manipulowanie tekstami w sposób bardziej precyzyjny i elastyczny niż standardowe operacje stringów. Regular expressions są szczególnie przydatne przy analizowaniu danych, parsowaniu dokumentów lub walidowaniu formularzy.

## Jak to zrobić?
```Swift
// Sprawdzenie, czy tekst zawiera numer telefonu w formacie (123)456-7890
let expression = "^\\(\\d{3}\\)\\d{3}-\\d{4}$"
let phoneRegex = try! NSRegularExpression(pattern: expression, options: [])
let phoneString = "(123)456-7890"

if phoneRegex.firstMatch(in: phoneString, options: [], range: NSRange(location: 0, length: phoneString.utf16.count)) != nil {
    print("Ten tekst zawiera poprawny numer telefonu!")
} else {
    print("Ten tekst nie zawiera poprawnego numeru telefonu.")
}
// Output: Ten tekst zawiera poprawny numer telefonu!
```

## Głębszy wgląd
Wyrażenia regularne zostały stworzone w latach 50. przez matematyka Stephena Cole Kleene. Dzięki nim, programiści mogą dziś w prosty sposób manipulować tekstami, wykorzystując ich logiczną ogólną strukturę. Alternatywami dla wyrażeń regularnych są np. parser-y lub metody specyficzne dla danego języka programowania. W Swift, regular expressions są wbudowane w framework NSRegularExpression.

## Zobacz także
1. [Dokumentacja wyrażeń regularnych w Swift](https://developer.apple.com/documentation/foundation/nsregularexpression)
2. [Wprowadzenie do wyrażeń regularnych w Swift](https://www.hackingwithswift.com/articles/108/a-swift-introduction-to-regular-expressions)