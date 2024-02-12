---
title:                "Создание временного файла"
aliases:
- /ru/swift/creating-a-temporary-file/
date:                  2024-01-28T23:57:22.679901-07:00
model:                 gpt-4-0125-preview
simple_title:         "Создание временного файла"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ru/swift/creating-a-temporary-file.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Что и Почему?
Создание временного файла представляет собой создание файлов с коротким временем жизни для хранения данных, которые не нужны на долгий срок. Программисты делают это для обработки данных, которые актуальны только во время выполнения программы, или чтобы не загромождать хранилище пользователя ненужными файлами.

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
