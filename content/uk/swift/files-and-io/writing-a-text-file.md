---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:29:55.411002-07:00
description: "\u0421\u0442\u0432\u043E\u0440\u0435\u043D\u043D\u044F \u0442\u0435\u043A\
  \u0441\u0442\u043E\u0432\u043E\u0433\u043E \u0444\u0430\u0439\u043B\u0443 \u0437\
  \u0430 \u0434\u043E\u043F\u043E\u043C\u043E\u0433\u043E\u044E Swift \u0434\u043E\
  \u0437\u0432\u043E\u043B\u044F\u0454 \u043F\u043E\u0441\u0442\u0456\u0439\u043D\u043E\
  \ \u0437\u0431\u0435\u0440\u0456\u0433\u0430\u0442\u0438 \u0440\u044F\u0434\u043A\
  \u043E\u0432\u0456 \u0434\u0430\u043D\u0456 \u043D\u0430 \u0444\u0430\u0439\u043B\
  \u043E\u0432\u0456\u0439 \u0441\u0438\u0441\u0442\u0435\u043C\u0456, \u0449\u043E\
  \ \u0454 \u0432\u0430\u0436\u043B\u0438\u0432\u0438\u043C \u0434\u043B\u044F \u0437\
  \u0430\u0432\u0434\u0430\u043D\u044C, \u0442\u0430\u043A\u0438\u0445 \u044F\u043A\
  \ \u0437\u0431\u0435\u0440\u0435\u0436\u0435\u043D\u043D\u044F\u2026"
lastmod: '2024-03-13T22:44:49.951246-06:00'
model: gpt-4-0125-preview
summary: "\u0421\u0442\u0432\u043E\u0440\u0435\u043D\u043D\u044F \u0442\u0435\u043A\
  \u0441\u0442\u043E\u0432\u043E\u0433\u043E \u0444\u0430\u0439\u043B\u0443 \u0437\
  \u0430 \u0434\u043E\u043F\u043E\u043C\u043E\u0433\u043E\u044E Swift \u0434\u043E\
  \u0437\u0432\u043E\u043B\u044F\u0454 \u043F\u043E\u0441\u0442\u0456\u0439\u043D\u043E\
  \ \u0437\u0431\u0435\u0440\u0456\u0433\u0430\u0442\u0438 \u0440\u044F\u0434\u043A\
  \u043E\u0432\u0456 \u0434\u0430\u043D\u0456 \u043D\u0430 \u0444\u0430\u0439\u043B\
  \u043E\u0432\u0456\u0439 \u0441\u0438\u0441\u0442\u0435\u043C\u0456, \u0449\u043E\
  \ \u0454 \u0432\u0430\u0436\u043B\u0438\u0432\u0438\u043C \u0434\u043B\u044F \u0437\
  \u0430\u0432\u0434\u0430\u043D\u044C, \u0442\u0430\u043A\u0438\u0445 \u044F\u043A\
  \ \u0437\u0431\u0435\u0440\u0435\u0436\u0435\u043D\u043D\u044F\u2026"
title: "\u041D\u0430\u043F\u0438\u0441\u0430\u043D\u043D\u044F \u0442\u0435\u043A\u0441\
  \u0442\u043E\u0432\u043E\u0433\u043E \u0444\u0430\u0439\u043B\u0443"
weight: 24
---

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
