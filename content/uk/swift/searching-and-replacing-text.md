---
title:                "Swift: Пошук та заміна тексту"
simple_title:         "Пошук та заміна тексту"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/swift/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Чому

Маніпуляція з текстом - це невідємна частина програмування і те, як надійно і швидко ви можете замінювати текст у своїх програмах, може суттєво впливати на їх продуктивність і ефективність.

## Як

Щоб замінити текст у Swift, використовуйте функцію `replacingOccurrences(of:with:)`. Наприклад:

```Swift
let str = "Привіт, світ!"
let newStr = str.replacingOccurrences(of: "Привіт", with: "Hello")
print(newStr)
// Виведе: Hello, світ!
```

## Deep Dive

Ви можете також використовувати регулярні вирази для складніших випадків заміни тексту. Для цього використовуйте функцію `replacingOccurrences(of:with:options:range:)`, де параметр `options` приймає значення `.regularExpression`. Наприклад:

```Swift
let str = "Hello, world!"
let newStr = str.replacingOccurrences(of: "[aeiou]", with: "*", options: .regularExpression, range: nil)
print(newStr)
// Виведе: H*ll*, w*rld!
```

## See Also

 - [Документація Apple з роботою зі строками в Swift](https://developer.apple.com/documentation/foundation/nsstring)
 - [Повний список опцій для регулярних виразів у Swift](https://developer.apple.com/documentation/foundation/nsregularexpression/options)