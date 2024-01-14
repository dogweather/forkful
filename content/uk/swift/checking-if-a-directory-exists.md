---
title:                "Swift: Перевірка існування каталогу"
simple_title:         "Перевірка існування каталогу"
programming_language: "Swift"
category:             "Swift"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/swift/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Чому
Перевірка наявності директорії є важливою частиною програмування Swift, оскільки вона дозволяє перевірити, чи існує певний каталог на пристрої перед виконанням певних дій з ним.

## Як це зробити
```Swift
let fileManager = FileManager.default
let documentsURL = fileManager.urls(for: .documentDirectory, in: .userDomainMask).first!
        
if fileManager.fileExists(atPath: documentsURL.path) {
    print("Каталог існує")
} else {
    print("Каталог не існує")
}
```

В цьому прикладі ми використовуємо `FileManager` для отримання шляху до директорії Document на пристрої. Потім ми перевіряємо, чи існує ця директорія за допомогою функції `fileExists(atPath:)` та виводимо відповідне повідомлення.

## Глибоке дослідження
Функція `fileExists(atPath:)` використовується для перевірки наявності файлу або директорії за заданим шляхом. Вона повертає значення типу `Bool`, яке вказує на те, чи існує заданий шлях. Ця функція також може бути використана для перевірки наявності інших файлів або директорій на пристрої.

## Дивись також
- [FileManager - документація Apple](https://developer.apple.com/documentation/foundation/filemanager)
- [Операції з файлами та директоріями в Swift](https://medium.com/swift-programming-club/working-with-files-and-directories-in-swift-5d1a4fee2542)
- [Swift File Handling Tutorial](https://www.raywenderlich.com/contentswp/6-swift-file-handling-tutorial-for-beginners)