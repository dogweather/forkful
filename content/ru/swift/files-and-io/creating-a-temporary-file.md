---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 23:57:22.679901-07:00
description: "\u041A\u0430\u043A \u044D\u0442\u043E \u0441\u0434\u0435\u043B\u0430\
  \u0442\u044C: Swift \u0434\u0435\u043B\u0430\u0435\u0442 \u0441\u043E\u0437\u0434\
  \u0430\u043D\u0438\u0435 \u0432\u0440\u0435\u043C\u0435\u043D\u043D\u044B\u0445\
  \ \u0444\u0430\u0439\u043B\u043E\u0432 \u0434\u043E\u0432\u043E\u043B\u044C\u043D\
  \u043E \u043F\u0440\u043E\u0441\u0442\u044B\u043C \u0441 \u043F\u043E\u043C\u043E\
  \u0449\u044C\u044E \u043A\u043B\u0430\u0441\u0441\u0430 `FileManager`. \u0412\u043E\
  \u0442 \u043A\u0430\u043A \u0431\u044B\u0441\u0442\u0440\u043E \u0441\u043E\u0437\
  \u0434\u0430\u0442\u044C \u0432\u0440\u0435\u043C\u0435\u043D\u043D\u044B\u0439\
  \ \u0444\u0430\u0439\u043B \u0438 \u0437\u0430\u043F\u0438\u0441\u0430\u0442\u044C\
  \u2026"
lastmod: '2024-03-13T22:44:45.718374-06:00'
model: gpt-4-0125-preview
summary: "Swift \u0434\u0435\u043B\u0430\u0435\u0442 \u0441\u043E\u0437\u0434\u0430\
  \u043D\u0438\u0435 \u0432\u0440\u0435\u043C\u0435\u043D\u043D\u044B\u0445 \u0444\
  \u0430\u0439\u043B\u043E\u0432 \u0434\u043E\u0432\u043E\u043B\u044C\u043D\u043E\
  \ \u043F\u0440\u043E\u0441\u0442\u044B\u043C \u0441 \u043F\u043E\u043C\u043E\u0449\
  \u044C\u044E \u043A\u043B\u0430\u0441\u0441\u0430 `FileManager`."
title: "\u0421\u043E\u0437\u0434\u0430\u043D\u0438\u0435 \u0432\u0440\u0435\u043C\u0435\
  \u043D\u043D\u043E\u0433\u043E \u0444\u0430\u0439\u043B\u0430"
weight: 21
---

## Как это сделать:
Swift делает создание временных файлов довольно простым с помощью класса `FileManager`. Вот как быстро создать временный файл и записать в него текст:

```Swift
import Foundation

// Создание URL временной директории
let tempDirectoryURL = FileManager.default.temporaryDirectory

// Создание уникального имени файла
let fileName = UUID().uuidString

// Построение полного URL файла
let fileURL = tempDirectoryURL.appendingPathComponent(fileName)

// Пример текста для записи
let sampleText = "Привет, временный мир!"

do {
    // Запись текста во временный файл
    try sampleText.write(to: fileURL, atomically: true, encoding: .utf8)
    print("Файл создан: \(fileURL)")
} catch {
    print("Не удалось записать файл: \(error)")
}

// Пример вывода:
// Файл создан: file:///path/to/temp/directory/E0B4952E-5BEE-47E7-B5BB-DA5E6AF1EDC9
```

Чтобы прочитать файл, просто сделайте всё наоборот—вот как:

```Swift
do {
    // Чтение текста из временного файла
    let savedText = try String(contentsOf: fileURL, encoding: .utf8)
    print("Содержимое файла: \(savedText)")
} catch {
    print("Не удалось прочитать файл: \(error)")
}

// Пример вывода:
// Содержимое файла: Привет, временный мир!
```

Подчистите за собой, удалив временный файл:

```Swift
do {
    // Удаление временного файла
    try FileManager.default.removeItem(at: fileURL)
    print("Временный файл удален.")
} catch {
    print("Не удалось удалить файл: \(error)")
}

// Пример вывода:
// Временный файл удален.
```

## Подробнее
До `FileManager` люди управляли файлами более громоздкими способами. Помните `tmpfile()` в C? `FileManager` в Swift в сравнении с ним - это просто и современно.

Альтернативы? Конечно. Вы можете использовать представления в памяти, такие как `Data` или `String`, идеально подходящие для действительно временных данных ограниченного размера. Другой путь - использование собственного менеджера временных файлов для большего контроля, но это обычно избыточно.

Самое важное: `FileManager` использует системную временную директорию, которая периодически очищается, но не после каждого запуска программы. Имейте это в виду, когда речь идет о безопасности или чувствительных данных — при необходимости очищайте вручную.

## Смотрите также
Посмотрите эти источники, чтобы узнать больше о работе с файлами в Swift:
- [Документация Apple о FileManager](https://developer.apple.com/documentation/foundation/filemanager)
- [Статья NSHipster о управлении файлами](https://nshipster.com/temporary-files/)
- [Руководство Ray Wenderlich по работе с файловой системой в Swift](https://www.raywenderlich.com/666-filemanager-class-tutorial-for-macos-getting-started)
