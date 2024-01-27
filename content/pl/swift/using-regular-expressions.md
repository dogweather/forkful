---
title:                "Wykorzystanie wyrażeń regularnych"
date:                  2024-01-19
html_title:           "Arduino: Wykorzystanie wyrażeń regularnych"
simple_title:         "Wykorzystanie wyrażeń regularnych"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/swift/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Co i dlaczego?
RegEx, czyli wyrażenia regularne, to wzorce szukania i manipulowania tekstami. Programiści używają ich do weryfikacji, podziału lub modyfikacji ciągów znaków, szybko i elastycznie.

## Jak to zrobić:
Wyrażenia regularne w Swift używasz poprzez `NSRegularExpression`. Przykład:

```Swift
import Foundation

let sampleText = "Jabłka są zielone i czerwone."

if let regex = try? NSRegularExpression(pattern: "\\bzielone\\b", options: .caseInsensitive) {
    let range = NSRange(location: 0, length: sampleText.utf16.count)
    if regex.firstMatch(in: sampleText, options: [], range: range) != nil {
        print("Znaleziono dopasowanie!")
    } else {
        print("Brak dopasowania.")
    }
}
```

Output:
```
Znaleziono dopasowanie!
```

Zamiana tekstu:

```Swift
let replacedText = regex.stringByReplacingMatches(in: sampleText, options: [], range: range, withTemplate: "pomarańczowe")
print(replacedText)
```

Output:
```
Jabłka są pomarańczowe i czerwone.
```

## Deep Dive
Wyrażenia regularne mają swoje korzenie w teorii języków formalnych i automatach – matematycy jak Stephen Kleene zdefiniowali je w latach 50. Alternatywnie, do manipulacji tekstami można wykorzystać metody `String`, ale są one mniej wydajne dla skomplikowanych wzorców. Implementacja RegEx w Swift używa bibliotek Foundation i jest kompatybilna z wyrażeniami regularnymi POSIX.

## See Also
Świetne źródła do nauki i eksperymentowania z wyrażeniami regularnymi:
- [NSRegularExpression Apple Documentation](https://developer.apple.com/documentation/foundation/nsregularexpression)
- [RegExr](https://regexr.com/) – narzędzie online do testowania wyrażeń regularnych.
- [Swift Algorithms Club - Regular Expressions Tutorial](https://www.raywenderlich.com/86205/nsregularexpression-swift-tutorial)
