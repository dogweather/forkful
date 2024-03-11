---
date: 2024-01-20 17:37:37.173674-07:00
description: "Co i dlaczego? Konwersja daty na ci\u0105g znak\xF3w umo\u017Cliwia\
  \ \u0142atwy wy\u015Bwietlanie i zapisywanie dat. Programi\u015Bci robi\u0105 to,\
  \ by formatowa\u0107 daty w spos\xF3b zrozumia\u0142y\u2026"
lastmod: '2024-03-11T00:14:08.972777-06:00'
model: gpt-4-1106-preview
summary: "Co i dlaczego? Konwersja daty na ci\u0105g znak\xF3w umo\u017Cliwia \u0142\
  atwy wy\u015Bwietlanie i zapisywanie dat. Programi\u015Bci robi\u0105 to, by formatowa\u0107\
  \ daty w spos\xF3b zrozumia\u0142y\u2026"
title: "Konwersja daty na \u0142a\u0144cuch znak\xF3w"
---

{{< edit_this_page >}}

## What & Why?
Co i dlaczego? Konwersja daty na ciąg znaków umożliwia łatwy wyświetlanie i zapisywanie dat. Programiści robią to, by formatować daty w sposób zrozumiały dla użytkowników i różnych systemów.

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
