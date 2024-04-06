---
date: 2024-01-20 17:37:50.049507-07:00
description: "\u042F\u043A \u0446\u0435 \u0437\u0440\u043E\u0431\u0438\u0442\u0438\
  : (How to:) ."
lastmod: '2024-04-05T21:53:50.008901-06:00'
model: gpt-4-1106-preview
summary: ''
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
