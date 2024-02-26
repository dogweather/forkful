---
date: 2024-01-20 17:58:51.246618-07:00
description: "Szukanie i zamiana tekstu to chleb powszedni w programowaniu: odnajdujemy\
  \ okre\u015Blone frazy i zmieniamy je na inne. Robimy to, gdy potrzebujemy masowo\u2026"
lastmod: '2024-02-25T18:49:34.112427-07:00'
model: gpt-4-1106-preview
summary: "Szukanie i zamiana tekstu to chleb powszedni w programowaniu: odnajdujemy\
  \ okre\u015Blone frazy i zmieniamy je na inne. Robimy to, gdy potrzebujemy masowo\u2026"
title: Wyszukiwanie i zamiana tekstu
---

{{< edit_this_page >}}

## Co i dlaczego?
Szukanie i zamiana tekstu to chleb powszedni w programowaniu: odnajdujemy określone frazy i zmieniamy je na inne. Robimy to, gdy potrzebujemy masowo poprawić dane, zaktualizować kod lub po prostu zautomatyzować nudne zadania edycyjne.

## Jak to zrobić:
```Swift
var text = "Witaj świecie! Swift jest fajny."
if let range = text.range(of: "świecie") {
   text.replaceSubrange(range, with: "Swift")
}
print(text) // "Witaj Swift! Swift jest fajny."

text = "Jabłka, banany, wiśnie."
text = text.replacingOccurrences(of: "wiśnie", with: "maliny")
print(text) // "Jabłka, banany, maliny."
```

## Zagłębiając się:
Historia poleceń wyszukiwania i zastępowania sięga wczesnych edytorów tekstu i systemów do przetwarzania tłumów, gdzie automatyzacja mogła zaoszczędzić godziny ręcznej pracy. W Swift, `String` oferuje metody jak `replacingOccurrences(of:with:)` czy `replaceSubrange(_:with:)`, które są intuicyjne w użyciu, ale bazują na potężnych mechanizmach porównywania tekstów, takich jak wyrażenia regularne. Alternatywne podejścia mogą obejmować użycie `NSRegularExpression` w swiftowych aplikacjach dla większej kontroli i elastyczności przy szukaniu wzorców.

## Zobacz również:
- Apple's Swift String Documentation: [Strings and Characters](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html)
- Regular Expressions in Swift: [NSRegularExpression](https://developer.apple.com/documentation/foundation/nsregularexpression)
