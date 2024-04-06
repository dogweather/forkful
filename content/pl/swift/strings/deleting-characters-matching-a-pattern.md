---
date: 2024-01-20 17:43:19.379717-07:00
description: "How to: (Jak to zrobi\u0107:) ."
lastmod: '2024-04-05T21:53:37.165408-06:00'
model: gpt-4-1106-preview
summary: ''
title: "Usuwanie znak\xF3w pasuj\u0105cych do wzorca"
weight: 5
---

## How to: (Jak to zrobić:)
```Swift
import Foundation

var email = "example+filter@domain.com"
let filteredEmail = email.replacingOccurrences(of: "\\+.*@", with: "@", options: .regularExpression)
print(filteredEmail) // Wypisze "example@domain.com"
```

```Swift
import Foundation

var filePath = "/User/Downloads/Some_file(backup).txt"
let cleanPath = filePath.replacingOccurrences(of: "\\(.*\\)", with: "", options: .regularExpression)
print(cleanPath) // Wypisze "/User/Downloads/Some_file.txt"
```

## Deep Dive (Dogłębna analiza)
Usunięcie znaków pasujących do określonego wzorca jest techniką regulowaną przez wyrażenia regularne (regex), które narodziły się w latach 50. Teoria automatów i języków formalnych była inspiracją. Teraz, używając Swifta, możemy przefiltrować stringi szybko i skutecznie.

Alternatywą jest własna funkcja skanująca znaki i budująca nowy string. Ale po co to robić, skoro mamy regex? Złożoność implementacyjna jest tam duża i łatwo o błąd.

Użycie `replacingOccurrences` z `options: .regularExpression` w Swift pozwala wykorzystywać moce regex bez komplikacji. String staje się elastyczny i łatwiejszy do ujarzmienia.

## See Also (Zobacz również)
- Apple's Swift documentation on String: [Strings and Characters](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html)
- Regular Expression Syntax Reference: [NSRegularExpression](https://developer.apple.com/documentation/foundation/nsregularexpression)
