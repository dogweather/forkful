---
title:    "Swift: Читання текстового файлу"
keywords: ["Swift"]
---

{{< edit_this_page >}}

# Зачем

Читати текстовий файл може бути корисним для вивчення різних програмних мов та практикування їх використання. Також, це може допомогти позбутися від навантаженого графічного інтерфейсу та більше сконцентруватися на програмуванні.

# Як

Для читання текстового файлу використовується метод `String(contentsOf: URL)`, що дозволяє прочитати вміст файлу за допомогою URL об'єкта. У наступному прикладі ми використаємо цей метод, щоб прочитати вміст файлу "text.txt" та вивести його в консоль:

```Swift
if let fileURL = Bundle.main.url(forResource: "text", withExtension: "txt") {
    do {
        let fileContents = try String(contentsOf: fileURL)
        print(fileContents)
    } catch {
        print("Error reading file: \(error)")
    }
}
```

Результатом буде виведення вмісту файлу "text.txt" в консоль.

# Глибше вдивимося

Крім `String(contentsOf: URL)`, існують й інші способи читання текстових файлів в Swift, наприклад використання `FileHandle` або укороченого синтаксису `try?` для простішого коду.

Для більшої ефективності, також можна використовувати `DispatchIO`, який дозволяє асинхронно читати великі файли та ефективно керувати пам'яттю.

# Дивіться також

- [Офіційна документація Swift для читання та запису файлів](https://docs.swift.org/swift-book/LanguageGuide/BasicOperators.html#writing-equality-operators-for-custom-types)
- [Стаття "Робота з файлами в Swift"](https://medium.com/@sgcsharp/work-with-files-in-swift-24640bc7e44a) в блозі Medium
- [Відеоурок "Читання та запис файлів в Swift"](https://www.youtube.com/watch?v=MElcVK1mIxo) на YouTube