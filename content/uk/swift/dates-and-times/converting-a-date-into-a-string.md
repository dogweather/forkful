---
title:                "Перетворення дати в рядок"
aliases:
- /uk/swift/converting-a-date-into-a-string/
date:                  2024-01-20T17:37:50.049507-07:00
model:                 gpt-4-1106-preview
simple_title:         "Перетворення дати в рядок"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/swift/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Що це та навіщо? (What & Why?)
Перетворення дати в рядок дає змогу зручно відображати і зберігати дати в текстовому форматі. Програмісти роблять це для легкого читання людиною чи форматування дат під конкретні локалізації та стандарти.

## Як це зробити: (How to:)
```Swift
import Foundation

// Створення об'єкта дати
let now = Date()

// Налаштування форматтера дати
let dateFormatter = DateFormatter()
dateFormatter.dateStyle = .medium
dateFormatter.timeStyle = .short
dateFormatter.locale = Locale(identifier: "uk_UA")

// Конвертація дати в рядок
let dateAsString = dateFormatter.string(from: now)
print(dateAsString) // Виводить, наприклад, "4 квіт. 2023 р., 14:37"
```

## Занурення в деталі (Deep Dive)
Перетворення дат у рядки є важливою частиною обробки дат з часів початку комп'ютерної ери. В Swift цей процес спростився завдяки вбудованим класам, як `DateFormatter`. До альтернатив належать бібліотеки сторонніх розробників, але `DateFormatter` достатньо потужний для більшості сценаріїв. Щодо деталей, важливо правильно налаштувати локаль (`locale`) та формат дати (`dateStyle` та `timeStyle`), щоб результат був коректним в контексті цільової аудиторії.

## Додаткові ресурси (See Also)
- [DateFormatter | Apple Developer Documentation](https://developer.apple.com/documentation/foundation/dateformatter)
- [Working With Date and Time | Ray Wenderlich](https://www.raywenderlich.com/5539282-working-with-date-and-time-in-ios-using-date-datecomponents-and-dateformatter)
- [Locale Class | Apple Developer Documentation](https://developer.apple.com/documentation/foundation/locale)
