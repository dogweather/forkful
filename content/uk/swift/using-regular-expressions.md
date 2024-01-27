---
title:                "Використання регулярних виразів"
date:                  2024-01-19
html_title:           "Bash: Використання регулярних виразів"
simple_title:         "Використання регулярних виразів"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/swift/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Що це таке & Навіщо?
Регулярні вирази - це шаблони для пошуку та маніпуляції текстом. Програмісти використовують їх для ефективної роботи зі строками - валідації, пошуку, заміни та аналізу даних.

## Як це робити:
```swift
import Foundation

let text = "hello@example.com"
let regexPattern = "^[a-z0-9.]+@[a-z]+\\.[a-z]{2,}$"

if let regex = try? NSRegularExpression(pattern: regexPattern) {
    let range = NSRange(location: 0, length: text.utf16.count)
    if regex.firstMatch(in: text, options: [], range: range) != nil {
        print("Valid email.")
    } else {
        print("Invalid email.")
    }
} else {
    print("Invalid regex pattern.")
}
```
#### Вихідні дані:
```
Valid email.
```

## Поглиблений розгляд:
Регулярні вирази беруть свій початок із теоретичної інформатики і були вперше введені в 1950х. Як альтернативу можна використовувати string методи `contains`, `range(of:)` або `hasPrefix`. На практиці, `NSRegularExpression` у Swift - це обгортка навколо ICU (International Components for Unicode), що забезпечує розширені можливості для роботи з регулярними виразами.

## Див. також:
- Документація Apple про NSRegularExpression: [Пряме посилання](https://developer.apple.com/documentation/foundation/nsregularexpression)
- Online regex tester (для випробування та освоєння регулярних виразів): [Ресурс](https://regexr.com)
