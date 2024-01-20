---
title:                "Аналіз дати з рядка"
date:                  2024-01-20T15:39:03.370118-07:00
html_title:           "Arduino: Аналіз дати з рядка"
simple_title:         "Аналіз дати з рядка"
programming_language: "Swift"
category:             "Swift"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/swift/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## What & Why?
## Що таке й навіщо?
Парсинг дати зі строки — це процес перетворення текстової інформації про дату у формат, що можна використати в коді. Програмісти роблять це, щоб легко маніпулювати датами і часом, забезпечувати логіку бізнес-операцій та працювати з API, які передають дати в текстових форматах.

## How to:
## Як робити:
```Swift
import Foundation

// Створюємо DateFormatter
let formatter = DateFormatter()
formatter.dateFormat = "yyyy-MM-dd HH:mm:ss"

// Строка дати для парсингу
let dateString = "2023-03-14 15:09:26"

// Парсимо строку в Date
if let date = formatter.date(from: dateString) {
    print("Дата успішно спарсена: \(date)")
} else {
    print("Не вдалося спарсити дату.")
}

// Вивід: Дата успішно спарсена: 2023-03-14 15:09:26 +0000
```
Тут ми використовуємо `DateFormatter` для конвертації строки у дату. Зверніть увагу: шаблон в `dateFormat` має відповідати формату вашої строчки.

## Deep Dive:
## Підводні камені:
Історично парсинг дат був завданням з купою пасток через різні формати дат і часові зони. Swift пропонує `DateFormatter`, що базується на ICU (International Components for Unicode). 

Альтернативи? ISO8601DateFormatter для ISO 8601, unix timestamps, або навіть користувацькі розширення `Date` класу якщо є специфічні потреби.

Під капотом, коли викликається `date(from:)`, `DateFormatter` розбирає строку крок за кроком, використовуючи заданий шаблон і локаль. Часова зона може бути налаштована, щоб впевнитися, що дата інтерпретується правильно. 

## See Also:
## Дивіться також:
- [Apple Developer Documentation for DateFormatter](https://developer.apple.com/documentation/foundation/dateformatter)
- [Working with Dates and Times in Swift (raywenderlich.com)](https://www.raywenderlich.com/5539282-working-with-dates-and-times-in-swift)
- [Unicode Date Format Patterns](https://unicode.org/reports/tr35/tr35-dates.html#Date_Format_Patterns)