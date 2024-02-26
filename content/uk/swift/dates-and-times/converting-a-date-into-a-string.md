---
date: 2024-01-20 17:37:50.049507-07:00
description: "\u041F\u0435\u0440\u0435\u0442\u0432\u043E\u0440\u0435\u043D\u043D\u044F\
  \ \u0434\u0430\u0442\u0438 \u0432 \u0440\u044F\u0434\u043E\u043A \u0434\u0430\u0454\
  \ \u0437\u043C\u043E\u0433\u0443 \u0437\u0440\u0443\u0447\u043D\u043E \u0432\u0456\
  \u0434\u043E\u0431\u0440\u0430\u0436\u0430\u0442\u0438 \u0456 \u0437\u0431\u0435\
  \u0440\u0456\u0433\u0430\u0442\u0438 \u0434\u0430\u0442\u0438 \u0432 \u0442\u0435\
  \u043A\u0441\u0442\u043E\u0432\u043E\u043C\u0443 \u0444\u043E\u0440\u043C\u0430\u0442\
  \u0456. \u041F\u0440\u043E\u0433\u0440\u0430\u043C\u0456\u0441\u0442\u0438 \u0440\
  \u043E\u0431\u043B\u044F\u0442\u044C \u0446\u0435 \u0434\u043B\u044F \u043B\u0435\
  \u0433\u043A\u043E\u0433\u043E \u0447\u0438\u0442\u0430\u043D\u043D\u044F \u043B\
  \u044E\u0434\u0438\u043D\u043E\u044E \u0447\u0438\u2026"
lastmod: '2024-02-25T18:49:47.352067-07:00'
model: gpt-4-1106-preview
summary: "\u041F\u0435\u0440\u0435\u0442\u0432\u043E\u0440\u0435\u043D\u043D\u044F\
  \ \u0434\u0430\u0442\u0438 \u0432 \u0440\u044F\u0434\u043E\u043A \u0434\u0430\u0454\
  \ \u0437\u043C\u043E\u0433\u0443 \u0437\u0440\u0443\u0447\u043D\u043E \u0432\u0456\
  \u0434\u043E\u0431\u0440\u0430\u0436\u0430\u0442\u0438 \u0456 \u0437\u0431\u0435\
  \u0440\u0456\u0433\u0430\u0442\u0438 \u0434\u0430\u0442\u0438 \u0432 \u0442\u0435\
  \u043A\u0441\u0442\u043E\u0432\u043E\u043C\u0443 \u0444\u043E\u0440\u043C\u0430\u0442\
  \u0456. \u041F\u0440\u043E\u0433\u0440\u0430\u043C\u0456\u0441\u0442\u0438 \u0440\
  \u043E\u0431\u043B\u044F\u0442\u044C \u0446\u0435 \u0434\u043B\u044F \u043B\u0435\
  \u0433\u043A\u043E\u0433\u043E \u0447\u0438\u0442\u0430\u043D\u043D\u044F \u043B\
  \u044E\u0434\u0438\u043D\u043E\u044E \u0447\u0438\u2026"
title: "\u041F\u0435\u0440\u0435\u0442\u0432\u043E\u0440\u0435\u043D\u043D\u044F \u0434\
  \u0430\u0442\u0438 \u0432 \u0440\u044F\u0434\u043E\u043A"
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
