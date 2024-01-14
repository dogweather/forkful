---
title:                "Swift: Перевірка наявності каталогу"
programming_language: "Swift"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/swift/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Чому

В програмуванні існує багато завдань, які вимагають перевірки наявності певного каталогу. Наприклад, може знадобитися перевірка наявності файла перед його відкриттям або збереженням. Це допоможе уникнути помилок та забезпечити правильне виконання програми. 

## Як це зробити

Існує кілька способів перевірити наявність директорії(каталогу) в Swift. Розглянемо два з них за допомогою коду та виводу:

```Swift
// Спосіб 1: Використовуючи функцію FileManager.default.fileExists(atPath: )

let fileManager = FileManager.default
let directoryPath = "/Users/user/Documents"

// перевірка наявності каталогу за допомогою функції fileExists(atPath: )
if fileManager.fileExists(atPath: directoryPath) {
    print("Каталог існує!")
} else {
    print("Каталог не існує!")
}

```
Вивід:

```Swift
Каталог існує!
```

```Swift
// Спосіб 2: Використовуючи функцію FileManager.default. urls(for: , in: )

let fileManager = FileManager.default
let directoryPath = "/Users/user/Documents"

// отримуємо URL каталогу за допомогою функції urls(for: , in: )
if let directoryURL = fileManager.urls(for: .documentDirectory, in: .userDomainMask).first {
    // перевірка наявності каталогу
    if fileManager.fileExists(atPath: directoryURL.appendingPathComponent(directoryPath).path) {
        print("Каталог існує!")
    } else {
        print("Каталог не існує!")
    }
}

```
Вивід:

```Swift
Каталог не існує!
```

## Глибокий аналіз

У Swift існує багато інших методів та функцій для перевірки наявності каталогу, таких як `fileExists(atPath: isDirectory:)`, `isReadableFile(atPath: )`, `isDeletableFile(atPath: )` та інші. Детальніше про ці методи та їх використання можна дізнатися в [офіційній документації Swift](https://developer.apple.com/documentation/foundation/filemanager). Також варто пам’ятати про використання обробки помилок при роботі з файлами та каталогами, щоб уникнути непередбачених ситуацій.

## Дивись також

- [Перевірка існування файла в Swift](https://github.com/apple/swift-evolution/blob/master/proposals/0116-filename-conventions.md)
- [Робота з файлами та каталогами в Swift](https://medium.com/@surjeetsngh159/working-with-files-and-directories-in-swift-9227bae80335)