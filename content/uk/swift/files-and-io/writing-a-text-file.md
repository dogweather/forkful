---
title:                "Написання текстового файлу"
aliases:
- /uk/swift/writing-a-text-file/
date:                  2024-02-03T19:29:55.411002-07:00
model:                 gpt-4-0125-preview
simple_title:         "Написання текстового файлу"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/swift/writing-a-text-file.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Що та чому?

Створення текстового файлу за допомогою Swift дозволяє постійно зберігати рядкові дані на файловій системі, що є важливим для завдань, таких як збереження налаштувань конфігурації, даних користувача чи журналів. Програмісти часто роблять це, щоб підтримувати дані між запусками додатку, обмінюватися даними між різними частинами додатку або експортувати дані для використання іншими програмами.

## Як це зробити:

### Використання стандартної бібліотеки Swift

Стандартна бібліотека Swift включає всі необхідні інструменти для запису текстових файлів. Ось базова методика:

```swift
import Foundation

let content = "Привіт, читачі Wired! Вивчення Swift веселе."
let filePath = NSSearchPathForDirectoriesInDomains(.documentDirectory, .userDomainMask, true)[0] as String
let fileName = "\(filePath)/example.txt"

do {
    try content.write(toFile: fileName, atomically: false, encoding: String.Encoding.utf8)
    print("Файл успішно записано")
} catch let error as NSError {
    print("Помилка запису за URL: \(fileName), Помилка: " + error.localizedDescription)
}
```

Цей фрагмент коду записує рядок у файл під назвою `example.txt` у каталозі документів. Він обробляє потенційні помилки за допомогою обробки помилок Swift у вигляді do-try-catch.

### Використання FileManager для більшого контролю

Для більшого контролю над атрибутами файлу або перевірки наявності файлу можна використовувати `FileManager`:

```swift
import Foundation

let fileManager = FileManager.default
let directories = fileManager.urls(for: .documentDirectory, in: .userDomainMask)
if let documentDirectory = directories.first {
    let fileURL = documentDirectory.appendingPathComponent("example.txt")
    let content = "Дослідження Swift для управління файлами є просвітливим."

    if fileManager.fileExists(atPath: fileURL.path) {
        print("Файл уже існує")
    } else {
        do {
            try content.write(to: fileURL, atomically: true, encoding: .utf8)
            print("Файл створено та успішно записано")
        } catch {
            print("Помилка запису файлу: \(error)")
        }
    }
}
```

### Використання сторонніх бібліотек

Одна з популярних сторонніх бібліотек для операцій з файловою системою в Swift - це `Files` від John Sundell:

Спочатку додайте Files до свого проекту, зазвичай через Swift Package Manager.

```swift
// swift-tools-version:5.3
import PackageDescription

let package = Package(
    name: "YourPackageName",
    dependencies: [
        .package(url: "https://github.com/JohnSundell/Files", from: "4.0.0"),
    ],
    targets: [
        .target(
            name: "YourTargetName",
            dependencies: ["Files"]),
    ]
)
```

Потім використовуйте її для запису в файл:

```swift
import Files

do {
    let file = try File(path: "/path/to/your/directory/example.txt")
    try file.write(string: "Swift і бібліотека Files створюють потужну комбінацію.")
    print("Файл успішно записано за допомогою бібліотеки Files.")
} catch {
    print("Сталась помилка: \(error)")
}
```

З бібліотекою `Files` робота з файлами стає простішою, що дозволяє вам зосередитись на бізнес-логіці вашого додатку, а не на нюансах управління файлами.
