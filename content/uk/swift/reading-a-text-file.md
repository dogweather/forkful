---
title:                "Читання текстового файлу"
aliases:
- uk/swift/reading-a-text-file.md
date:                  2024-01-20T17:55:20.384882-07:00
model:                 gpt-4-1106-preview
simple_title:         "Читання текстового файлу"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/swift/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Що & Чому?
Читання текстового файлу — це процес завантаження тексту з файлу у вашу програму. Програмісти це роблять, щоб обробити дані або зчитати конфігурації.

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
