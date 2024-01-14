---
title:    "Swift: Створення тимчасового файлу."
keywords: ["Swift"]
---

{{< edit_this_page >}}

## Чому

Створення тимчасових файлів є необхідною частиною багатьох програм. Вони дозволяють зберігати тимчасові дані, такі як кеш або буфери, щоб програма працювала більш ефективно.

## Як

Для створення тимчасового файлу у Swift, ми можемо використовувати клас `FileManager` та його метод `createTemporaryFile`. Нижче показано приклад, як створити тимчасовий файл і отримати його шлях:

```Swift
let fileManager = FileManager.default
do {
    let tempFileURL = try fileManager.createTemporaryFile()
    print(tempFileURL.path)
} catch {
    print("Сталася помилка при створенні тимчасового файлу: \(error)")
}
```

У вихідному коді ми використовуємо оператор `try` для обробки можливої помилки при створенні тимчасового файлу. Якщо усе пройшло успішно, ми можемо отримати шлях до створеного файлу і використовувати його у нашій програмі.

## Глибокий занурення

Іноді, ми можемо захотіти видалити тимчасовий файл після його використання. Для цього ми можемо використовувати метод `FileManager` `removeItem(at:)` для видалення файлу за його шляхом. Нижче показано приклад:

```Swift
let fileManager = FileManager.default
do {
    let tempFileURL = try fileManager.createTemporaryFile()
    print(tempFileURL.path)
    try fileManager.removeItem(at: tempFileURL)
} catch {
    print("Сталася помилка при створенні тимчасового файлу: \(error)")
}
```

Це допоможе нам уникнути залишків тимчасових файлів і звільнити місце на пристрої.

## Дивись також

- [Робота з файлами і директоріями у Swift](https://www.swiftbysundell.com/tips/files)
- [Офіційна документація Swift про роботу з файлами](https://docs.swift.org/swift-book/LanguageGuide/BasicOperators.html)