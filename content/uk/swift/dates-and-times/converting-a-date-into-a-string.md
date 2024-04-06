---
date: 2024-01-20 17:37:50.049507-07:00
description: "\u042F\u043A \u0446\u0435 \u0437\u0440\u043E\u0431\u0438\u0442\u0438\
  : (How to:) \u041F\u0435\u0440\u0435\u0442\u0432\u043E\u0440\u0435\u043D\u043D\u044F\
  \ \u0434\u0430\u0442 \u0443 \u0440\u044F\u0434\u043A\u0438 \u0454 \u0432\u0430\u0436\
  \u043B\u0438\u0432\u043E\u044E \u0447\u0430\u0441\u0442\u0438\u043D\u043E\u044E\
  \ \u043E\u0431\u0440\u043E\u0431\u043A\u0438 \u0434\u0430\u0442 \u0437 \u0447\u0430\
  \u0441\u0456\u0432 \u043F\u043E\u0447\u0430\u0442\u043A\u0443 \u043A\u043E\u043C\
  \u043F'\u044E\u0442\u0435\u0440\u043D\u043E\u0457 \u0435\u0440\u0438. \u0412 Swift\
  \ \u0446\u0435\u0439 \u043F\u0440\u043E\u0446\u0435\u0441 \u0441\u043F\u0440\u043E\
  \u0441\u0442\u0438\u0432\u0441\u044F \u0437\u0430\u0432\u0434\u044F\u043A\u0438\u2026"
lastmod: '2024-04-05T22:51:02.857005-06:00'
model: gpt-4-1106-preview
summary: "(How to:) \u041F\u0435\u0440\u0435\u0442\u0432\u043E\u0440\u0435\u043D\u043D\
  \u044F \u0434\u0430\u0442 \u0443 \u0440\u044F\u0434\u043A\u0438 \u0454 \u0432\u0430\
  \u0436\u043B\u0438\u0432\u043E\u044E \u0447\u0430\u0441\u0442\u0438\u043D\u043E\u044E\
  \ \u043E\u0431\u0440\u043E\u0431\u043A\u0438 \u0434\u0430\u0442 \u0437 \u0447\u0430\
  \u0441\u0456\u0432 \u043F\u043E\u0447\u0430\u0442\u043A\u0443 \u043A\u043E\u043C\
  \u043F'\u044E\u0442\u0435\u0440\u043D\u043E\u0457 \u0435\u0440\u0438."
title: "\u041F\u0435\u0440\u0435\u0442\u0432\u043E\u0440\u0435\u043D\u043D\u044F \u0434\
  \u0430\u0442\u0438 \u0432 \u0440\u044F\u0434\u043E\u043A"
weight: 28
---

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
