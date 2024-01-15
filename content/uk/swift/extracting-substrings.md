---
title:                "Видобуття підрядків"
html_title:           "Swift: Видобуття підрядків"
simple_title:         "Видобуття підрядків"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/swift/extracting-substrings.md"
---

{{< edit_this_page >}}

## Чому

Виділення підрядків є корисним інструментом для обробки рядків у Swift. Це дозволяє програмістам отримати частину тексту з рядка, що є корисним, коли необхідно виконувати операції з конкретною частиною даних.

## Як використовувати

Ви можете витягти підрядок у Swift за допомогою функції `substring`. Наприклад, якщо у вас є рядок `let message = "Hello, world!"`, ви можете використати `message.substring(from: 0, to: 5)` для отримання підрядка `"Hello"`. Ви також можете використовувати інші методи, такі як `prefix`, `suffix`, `index` для витягування підрядків з рядка.

```Swift
let message = "Hello, world!"
let substring = message.substring(from: 0, to: 5)
print(substring)
// Output: "Hello"
```

## Profundum Mergantur

Є кілька додаткових нюансів, що стосуються виділення підрядків у Swift. Наприклад, ви можете використовувати знаки інтервал між крапками для вказівки відступів з початку або кінця рядка. Також, рядки в Swift можуть містити багато мов, тому необхідно використовувати правильну функцію `substring` для отримання потрібного підрядка.

## Дивитися також

Для додаткової інформації про виділення підрядків у Swift, можна переглянути наступні посилання:

- [Офіційної документації Swift](https://developer.apple.com/library/archive/documentation/StringsAndCharacters/Conceptual/SwiftStringsAndCharacters/), яка пропонує більш детальне пояснення про використання `substring` і інших методів для роботи з рядками.
- [Блога Swift by Sundell](https://www.swiftbysundell.com/basics/working-with-strings-in-swift/), де можна знайти корисні поради та трюки для роботи з рядками у Swift.