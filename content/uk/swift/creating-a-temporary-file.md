---
title:                "Створення тимчасового файлу"
html_title:           "Swift: Створення тимчасового файлу"
simple_title:         "Створення тимчасового файлу"
programming_language: "Swift"
category:             "Swift"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/swift/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Чому

Іноді в процесі програмування нам потрібно створити тимчасовий файл для збереження часом непотрібної інформації або для тестування певної частини коду. Це може бути корисно, оскільки тимчасові файли автоматично видаляються після завершення виконання програми.

## Як

Один зі способів створити тимчасовий файл у Swift - використати функцію `URL`, яка дозволяє нам генерувати URL-адреси для файлів в нашій системі.

```Swift
// Створення шляху до тимчасового файлу
let temporaryFileURL = URL(fileURLWithPath: NSTemporaryDirectory()).appendingPathComponent("temporary_file.txt")

// Записуємо строку у тимчасовий файл
let text = "Це текст, який буде записаний у тимчасовий файл."
try text.write(to: temporaryFileURL, atomically: true, encoding: .utf8)

// Читаємо дані з тимчасового файлу та виводимо їх у консоль
let data = try Data(contentsOf: temporaryFileURL)
print(String(data: data, encoding: .utf8))
```

В результаті, ми отримаємо вивід у консолі:

```
Це текст, який буде записаний у тимчасовий файл.
```

## Глибше

Розглянемо кожен рядок коду ближче. У першому рядку ми використовуємо функцію `NSTemporaryDirectory()`, щоб отримати шлях до тимчасової директорії нашої системи, і потім додаємо до неї назву нашого тимчасового файлу. Далі, ми використовуємо метод `write(to:atomically:encoding:)`, щоб записати наш текстовий рядок у створений тимчасовий файл. У цьому методі, ми передаємо посилання на наш тимчасовий файл, аргумент `atomically` вказує, чи ми хочемо, щоб файл був збережений атомарно (тобто, якщо операція запису буде перервана, файл буде видалено), а `encoding` вказує, яке кодування ми хочемо використовувати для запису даних. У останньому рядку ми використовуємо метод `Data(contentsOf:)` для отримання даних з нашого тимчасового файлу та виводимо їх у консоль.

## Дивіться також

- Офіційна документація Swift про роботу з файлами: https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html
- Стаття про функцію `NSTemporaryDirectory()`: https://www.hackingwithswift.com/example-code/foundation/how-to-create-a-temporary-file-and-directory-using-the-temporarydirectory-method
- Туторіал про роботу з файлами у Swift: https://www.raywenderlich.com/9482-ios-file-management-tutorial-working-with-directories-in-swift