---
title:                "Створення текстового файлу"
html_title:           "Swift: Створення текстового файлу"
simple_title:         "Створення текстового файлу"
programming_language: "Swift"
category:             "Swift"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/swift/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Чому
Якщо ви хочете зберегти дані або текстову інформацію у зручному форматі, наприклад, для подальшого використання або обробки, написання текстового файлу у Swift може бути корисним інструментом. Це дозволяє зберігати дані на вашому пристрої та переглядати їх у будь-який момент.

## Як це зробити
Для початку, створіть змінну, яка міститиме шлях до вашого файлу:

```Swift
let filePath = FileManager.default.urls(for: .documentDirectory, in: .userDomainMask).first?.appendingPathComponent("example.txt")
```

Цей код дозволить вам отримати шлях до папки документів на вашому пристрої та додати назву вашого файлу до цього шляху.

Наступна крок - написання тексту, який ви хочете зберегти у файлі, у форматі `String`:

```Swift
let text = "Це приклад тексту, який буде збережено у файлі."
```

Тепер ви можете створити `Do-Catch` блок для обробки можливих помилок та записати ваш текстовий файл за допомогою `write(to:atomically:encoding:)` методу:

```Swift
do {
    try text.write(to: filePath!, atomically: true, encoding: .utf8)
    print("Текст успішно збережено у файлі.")
} catch {
    print("Помилка під час запису у файл.")
}
```

Якщо ви хочете зчитати вміст файлу назад, ви можете це зробити за допомогою `String(contentsOf:encoding:)` методу:

```Swift
do {
    let fileContent = try String(contentsOf: filePath!, encoding: .utf8)
    print(fileContent)
} catch {
    print("Помилка під час зчитування файлу.")
}
```

## Глибоке дослідження
Ще один метод для написання текстових файлів у Swift - це використання `FileHandle` класу. Він надає більше можливостей для роботи з файлами та дозволяє виконати більш розширені дії, такі як переміщення курсору чи зміна розміщення даних у файлі.

Запис у файл за допомогою `FileHandle` може виглядати так:

```Swift
do {
    if let fileHandle = FileHandle(forWritingAtPath: filePath!.path) {
        fileHandle.seekToEndOfFile()
        let data = text.data(using: .utf8)!
        fileHandle.write(data)
        fileHandle.closeFile()
        print("Текст успішно додано до файлу.")
    }
} catch {
    print("Помилка під час запису у файл з використанням FileHandle.")
}
```

Якщо ви хочете зчитати усі дані з файлу, ви можете використовувати `FileHandle` у режимі читання:

```Swift
do {
    if let fileHandle = FileHandle(forReadingAtPath: filePath!.path) {
        let fileData = fileHandle.readDataToEndOfFile()
        let fileContent = String(data: fileData, encoding: .utf8)
        print(fileContent)
        fileHandle.closeFile()
    }
} catch {
    print("Помилка під час зчитування файлу з використанням