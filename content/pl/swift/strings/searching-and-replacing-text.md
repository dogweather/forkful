---
date: 2024-01-20 17:58:51.246618-07:00
description: "Jak to zrobi\u0107: Historia polece\u0144 wyszukiwania i zast\u0119\
  powania si\u0119ga wczesnych edytor\xF3w tekstu i system\xF3w do przetwarzania t\u0142\
  um\xF3w, gdzie automatyzacja mog\u0142a\u2026"
lastmod: '2024-04-05T21:53:37.166232-06:00'
model: gpt-4-1106-preview
summary: "Historia polece\u0144 wyszukiwania i zast\u0119powania si\u0119ga wczesnych\
  \ edytor\xF3w tekstu i system\xF3w do przetwarzania t\u0142um\xF3w, gdzie automatyzacja\
  \ mog\u0142a zaoszcz\u0119dzi\u0107 godziny r\u0119cznej pracy."
title: Wyszukiwanie i zamiana tekstu
weight: 10
---

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
