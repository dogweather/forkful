---
title:                "Swift: Написання текстового файлу"
simple_title:         "Написання текстового файлу"
programming_language: "Swift"
category:             "Swift"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/swift/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Чому

У програмуванні часто потрібно працювати з текстовими файлами, які зберігають дані або виконують певні функції. Наприклад, це може бути файл зі збереженими налаштуваннями програми або файл логів для зберігання важливих подій. Написання текстового файлу дозволяє вам зберегти інформацію на жорсткому диску і зручно працювати з нею пізніше, не користуючись постійною пам'яттю.

## Як це зробити

Написання текстового файлу в Swift - це проста задача, яка використовує стандартний клас `FileManager`. Спочатку потрібно створити об'єкт цього класу, використовуючи ключове слово `let` для створення константи. Потім використовуйте цей об'єкт для створення і записування в файл потрібної інформації:

```Swift
let fileManager = FileManager()

do {
    let fileURL = URL(fileURLWithPath: "myfile.txt") // задайте назву і шлях до файлу
    try "Hello world".write(to: fileURL, atomically: true, encoding: .utf8) // записуємо рядок у файл
} catch {
    print("Помилка: \(error.localizedDescription)")
}
```

У цьому прикладі ми створюємо файл з назвою "myfile.txt" і записуємо в нього рядок "Hello world" з використанням кодування UTF-8. Параметр `atomically` визначає, чи потрібно використовувати механізм імовірного зберігання, щоб уникнути втрати даних при випадковому завершенні програми або при сбої в системі.

## Deep Dive

У даному розділі хочу надати вам додаткову інформацію про запис текстових файлів в Swift. Крім класу `FileManager`, існують інші способи створення і записування файлів. Наприклад, ви можете використовувати клас `NSString` замість рядка Swift, як це було показано в прикладі вище. Також можна використовувати клас `Data` для запису бінарних даних, які потім можна буде прочитати з файлу.

Для того, щоб перевірити, чи існує файл з певним ім'ям, можна скористатися методом `fileExists(atPath:)` та передати в нього шлях до файла.

## Дивіться також

- [Документація Apple про FileManager](https://developer.apple.com/documentation/foundation/filemanager)
- [Стаття про роботу з файлами в Swift](https://www.hackingwithswift.com/example-code/system/how-to-read-and-write-files-in-swift) від Hacking with Swift
- [Поради та приклади використання FileManager в Swift](https://www.swiftbysundell.com/basics/files-and-folders/) від Swift by Sundell.