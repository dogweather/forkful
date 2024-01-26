---
title:                "Створення текстового файлу"
html_title:           "Arduino: Створення текстового файлу"
simple_title:         "Створення текстового файлу"
programming_language: "Swift"
category:             "Swift"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/swift/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Що та чому?

Запис текстового файлу - це процес збереження даних у форматі, що може бути прочитаним людиною. Програмісти роблять це для логування, зберігання налаштувань, експорту даних.

## Як це зробити:

Запис файлу в Swift:

```Swift
import Foundation

let filename = "example.txt"
let directoryURL = FileManager.default.urls(for: .documentDirectory, in: .userDomainMask)[0]
let fileURL = directoryURL.appendingPathComponent(filename)

let content = "Привіт, це приклад тексту."
do {
    try content.write(to: fileURL, atomically: true, encoding: .utf8)
    print("Файл успішно записано!")
} catch {
    print("Помилка при записі файлу: \(error)")
}
```

Результат у консолі:

```
Файл успішно записано!
```

## Глибше занурення:

Запис файлу у Swift користується вбудованим функціоналом `FileManager` та `String` для роботи з файловою системою. Історично, запис файлів був складнішим з більш низькорівневими API як `fwrite` у C. Зміни в Swift, зокрема простота `String.write()`, спрощують процес. Альтернативами є використання `OutputStream`, CoreData або Realm для складніших даних.

## Дивись також:

- Офіційна документація Apple по роботі з файлами: [Working with Files in Swift](https://developer.apple.com/documentation/foundation/filemanager)
- Туторіал по запису текстових файлів в Swift: [Writing text to files in Swift](https://www.hackingwithswift.com/example-code/system/how-to-write-text-to-a-file-using-writeto)
- Додаткові опції файлового вводу-виводу у Swift: [File Handling in Swift](https://www.raywenderlich.com/2292-file-manager-class-tutorial-for-macos-getting-started-with-the-file-system-nsfilemanager)
