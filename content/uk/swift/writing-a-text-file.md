---
title:    "Swift: Написання текстового файлу"
keywords: ["Swift"]
---

{{< edit_this_page >}}

## Для чого

Написання текстового файлу є важливою складовою програмування на Swift. Це дозволяє зберігати дані, які можна використовувати в подальшій роботі з програмою.

## Як це зробити

Для написання текстового файлу потрібно використовувати функцію `write(to: atomically: encoding:)`, де `to` - це шлях до файлу, `atomically` - вказує, чи файл має бути збережений атомарно, `encoding` - кодування, яке буде використовуватись для збереження тексту.

Давайте розглянемо приклад коду, який демонструє написання текстового файлу:

```Swift
let content = "Привіт! Це мій перший текстовий файл."
let fileURL = URL(fileURLWithPath: "/Users/User/Documents/file.txt")
    
do {
    try content.write(to: fileURL, atomically: false, encoding: .utf8)
    print("Файл успішно збережено.")
} catch {
    print("Помилка при збереженні файлу: \(error)")
}
```

Після виконання цього коду, у вашій папці "Documents" з'явиться файл з назвою "file.txt" і текстом "Привіт! Це мій перший текстовий файл.".

## Глибоке занурення

Є декілька речей, про які варто пам'ятати при написанні текстових файлів на Swift:

- Функція `write(to: atomically: encoding:)` використовує блокування файлів, тому краще використовувати її у фоновому потоці.
- Якщо ви хочете додати текст до вже існуючого файлу, вам потрібно використовувати функцію `append()`, а не `write(to:)`.
- Для отримання тексту з текстового файлу використовуйте функцію `String(contentsOf: encoding:)`.

## Дивись також

- [Документація Swift по роботі з файлами](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html#ID381)
- [Стаття про роботу з текстовими файлами на Swift](https://www.hackingwithswift.com/example-code/strings/how-to-save-a-string-to-a-file-on-disk-with-write)
- [Створення файлів на Swift](https://learnappmaking.com/write-text-files-swift-string-contents/)