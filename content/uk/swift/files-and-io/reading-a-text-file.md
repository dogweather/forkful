---
date: 2024-01-20 17:55:20.384882-07:00
description: "\u042F\u043A \u0446\u0435 \u0437\u0440\u043E\u0431\u0438\u0442\u0438\
  : \u0427\u0438\u0442\u0430\u043D\u043D\u044F \u0444\u0430\u0439\u043B\u0456\u0432\
  \ \u0442\u0435\u043A\u0441\u0442\u0443 \u0432 Swift \u043D\u0435 \u0432\u0438\u043D\
  \u0438\u043A\u043B\u043E \u0437 \u043F\u0443\u0441\u0442\u043E\u0442\u0438; \u0446\
  \u0435\u0439 \u043F\u0440\u043E\u0446\u0435\u0441 \u0441\u044F\u0433\u0430\u0454\
  \ \u043A\u043E\u0440\u0456\u043D\u043D\u044F\u043C \u0432 \u0440\u0430\u043D\u043D\
  \u0456 \u0434\u043D\u0456 \u043F\u0440\u043E\u0433\u0440\u0430\u043C\u0443\u0432\
  \u0430\u043D\u043D\u044F. \u0406\u0441\u0442\u043E\u0440\u0438\u0447\u043D\u043E\
  , \u0447\u0438\u0442\u0430\u043D\u043D\u044F \u0434\u0430\u043D\u0438\u0445 \u0431\
  \u0443\u043B\u043E\u2026"
lastmod: '2024-04-05T22:51:02.864825-06:00'
model: gpt-4-1106-preview
summary: "\u0427\u0438\u0442\u0430\u043D\u043D\u044F \u0444\u0430\u0439\u043B\u0456\
  \u0432 \u0442\u0435\u043A\u0441\u0442\u0443 \u0432 Swift \u043D\u0435 \u0432\u0438\
  \u043D\u0438\u043A\u043B\u043E \u0437 \u043F\u0443\u0441\u0442\u043E\u0442\u0438\
  ; \u0446\u0435\u0439 \u043F\u0440\u043E\u0446\u0435\u0441 \u0441\u044F\u0433\u0430\
  \u0454 \u043A\u043E\u0440\u0456\u043D\u043D\u044F\u043C \u0432 \u0440\u0430\u043D\
  \u043D\u0456 \u0434\u043D\u0456 \u043F\u0440\u043E\u0433\u0440\u0430\u043C\u0443\
  \u0432\u0430\u043D\u043D\u044F."
title: "\u0427\u0438\u0442\u0430\u043D\u043D\u044F \u0442\u0435\u043A\u0441\u0442\u043E\
  \u0432\u043E\u0433\u043E \u0444\u0430\u0439\u043B\u0443"
weight: 22
---

## Як це зробити:
```Swift
import Foundation

// Путь до файлу
let path = "/path/to/your/file.txt"

// Читання вмісту з файла
if let content = try? String(contentsOfFile: path, encoding: .utf8) {
    print(content)
} else {
    print("Не можливо прочитати файл.")
}
```
Вихідні дані:
```
Привіт! Це текст з вашого файла.
```

## Поглиблено:
Читання файлів тексту в Swift не виникло з пустоти; цей процес сягає корінням в ранні дні програмування. Історично, читання даних було одним із перших завдань, які комп’ютери виконували. Ви могли б використовувати InputStream або FileManager для більш складного читання, а також налаштувати потоки для асинхронного читання. Важливо розуміти, як Swift управляє пам'яттю під час операцій з файлами, щоб не зіткнутися з витоками пам'яті або виключеннями під час роботи з великими файлами.

## Дивіться також:
- Apple Documentation for FileManager: [FileManager](https://developer.apple.com/documentation/foundation/filemanager)
- Apple Documentation for Streams: [Streams](https://developer.apple.com/documentation/foundation/stream)
- Swift String and Characters: [Strings and Characters](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html)
