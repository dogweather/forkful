---
date: 2024-01-20 17:37:37.173674-07:00
description: "How to: Jak to zrobi\u0107? U\u017Cyj `DateFormatter` w Swift, aby przekszta\u0142\
  ci\u0107 `Date` w `String`."
lastmod: '2024-04-05T21:53:37.193054-06:00'
model: gpt-4-1106-preview
summary: ''
title: "Konwersja daty na \u0142a\u0144cuch znak\xF3w"
weight: 28
---

## How to:
Jak to zrobić? Użyj `DateFormatter` w Swift, aby przekształcić `Date` w `String`.

```Swift
import Foundation

let now = Date()
let formatter = DateFormatter()

// Format: yyyy-MM-dd HH:mm:ss
formatter.dateFormat = "yyyy-MM-dd HH:mm:ss"
let dateString = formatter.string(from: now)
print(dateString) // 2023-04-01 15:46:10
```

Wynik: Data i czas w formacie: rok-miesiąc-dzień godzina:minuta:sekunda

## Deep Dive:
Wyjaśnienia: `DateFormatter` pojawił się w Swift wraz z jego początkami jako most do `NSDateFormatter` z Objective-C. W alternatywie: można użyć bibliotek zewnętrznych jak SwiftDate lub użyć `ISO8601DateFormatter` dla formatów ISO 8601. Ważne jest, aby pamiętać, że konwersja dat na ciągi znaków i odwrotnie może być kosztowna pod względem wydajności, więc nie nadużywaj tej operacji w gorących ścieżkach kodu.

## See Also:
Zobacz również:

- Apple's DateFormatter documentation: [https://developer.apple.com/documentation/foundation/dateformatter](https://developer.apple.com/documentation/foundation/dateformatter)
- SwiftDate, a powerful date library for Swift: [https://github.com/malcommac/SwiftDate](https://github.com/malcommac/SwiftDate)
- An article on date and time best practices: [https://nsdateformatter.com](https://nsdateformatter.com)
