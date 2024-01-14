---
title:                "Swift: Створення тимчасового файлу"
programming_language: "Swift"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/swift/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Чому

Створення тимчасового файлу - це корисна техніка, яку можна використовувати в Swift програмуванні для тимчасового збереження даних. Наприклад, ви можете створити тимчасовий файл для збереження результатів обчислень або для тимчасового збереження користувацьких налаштувань, які необхідно буде видалити після використання. 

## Як

Для створення тимчасового файлу у Swift використовуйте зручний клас `FileManager` і його метод `URLForDirectory`. Використовуючи дані методи, ви можете створити тимчасовий шлях для файлу та зменшити шанси на конфлікт з іншими файлами, зберігаючи його у рандомній папці. Ось приклад коду, який створює тимчасовий файл і виводить його шлях:

```Swift
let fileManager = FileManager.default
let tempDir = fileManager.temporaryDirectory
let tempFileName = "\(UUID().uuidString).txt" //робить унікальне ім'я
let tempURL = tempDir.appendingPathComponent(tempFileName) //створює шлях до файлу

do {
    try "Hello World".write(to: tempURL, atomically: true, encoding: .utf8)
    print("Шлях до тимчасового файлу:  \(tempURL.path)")
} catch {
    print("Помилка створення тимчасового файлу: \(error.localizedDescription)")
}

// Output: Шлях до тимчасового файлу: "/var/folders/z9/j7_6g76j3hbcyl5nkzmzh7cw0000gn/T/15D8E674-E9FB-40DA-B01C-E69DC904239C.txt"
```

## Deep Dive

Використання тимчасового файлу може бути корисною технікою для оптимізації виконання програми. Наприклад, якщо ваша програма працює з великими об'ємами даних, використання тимчасового файлу може допомогти зменшити навантаження на пам'ять шляхом часткового збереження даних у файл. Також, використання тимчасового файлу може бути корисним для збереження альтернативних версій даних при роботі з мережею або іншими зовнішніми джерелами.

## Дивіться також

- [Apple Documentation on FileManager](https://developer.apple.com/documentation/foundation/filemanager)
- [Swift Documentation on URL](https://developer.apple.com/documentation/foundation/url)
- [Article on GeeksforGeeks about Temporary File in Swift](https://www.geeksforgeeks.org/create-a-temporary-file-in-swift/)
- [Stack Overflow Discussion on Creating a Temporary File in Swift](https://stackoverflow.com/questions/39620266/create-a-temporary-file-in-swift)