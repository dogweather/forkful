---
title:                "Создание временного файла"
html_title:           "Swift: Создание временного файла"
simple_title:         "Создание временного файла"
programming_language: "Swift"
category:             "Swift"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/swift/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Що & Чому?
Створення тимчасових файлів - це процес створення файлів, які використовуються тимчасово під час виконання програми. Програмісти часто використовують такі файли для збереження проміжних результатів або для тимчасового збереження даних.

## Як це зробити:
```Swift
// Створення тимчасового файлу з використанням унікального імені
let temporaryFileURL = URL(fileURLWithPath: NSTemporaryDirectory())
    .appendingPathComponent(UUID().uuidString)

// Запис даних в тимчасовий файл
try "Hello World".write(to: temporaryFileURL, atomically: true, encoding: .utf8)

// Читання даних з тимчасового файлу
let data = try Data(contentsOf: temporaryFileURL)
print(String(data: data, encoding: .utf8)!) // Output: Hello World

// Видалення тимчасового файлу
try FileManager.default.removeItem(at: temporaryFileURL)
```

## Поглиблене вивчення:
Створення тимчасових файлів датується з 1970-х років, коли ОС Unix почала використовувати їх для оптимізації роботи з дисковим простором. Варіанти створення тимчасових файлів можуть відрізнятись залежно від платформи та мови програмування. Наприклад, у Java та C# цей процес виконується використовуючи класи "File.createTempFile" та "Path.GetTempFileName" відповідно.

## Дивіться також:
- [Створення тимчасового файлу в Swift](https://www.avanderlee.com/swift/temporary-files/)
- [Стаття блогу "Про тимчасові файли"](https://www.wendy.dev/blog/temporary-files/