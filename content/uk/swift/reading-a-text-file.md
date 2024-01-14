---
title:    "Swift: Читання текстового файлу"
keywords: ["Swift"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/uk/swift/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Чому

Програмісти часто потребують читати дані з текстових файлів для подальшої обробки та аналізу. Це може бути необхідно для роботи з великими обсягами даних або збереження інформації у структурованому форматі. Читання текстових файлів є важливою складовою будь-якої програми, яка працює зі збереженими даними, тому необхідно знати, як це зробити правильно.

## Як це зробити

Найпростіший спосіб прочитати текстовий файл у Swift - використовувати клас `String`. Нижче показано приклад читання текстового файлу та виведення його вмісту:

```Swift
if let path = Bundle.main.path(forResource: "file", ofType: "txt") { // шлях до файлу
    do {
        let content = try String(contentsOfFile: path, encoding: .utf8)
        print(content) // виводимо текстовий файл
    } catch {
        print("Помилка читання файлу: \(error)")
    }
}
```

В даному прикладі ми отримуємо шлях до файлу, використовуючи `Bundle` і метод `path(forResource:ofType:)`. Після цього за допомогою класу `String` ми зчитуємо вміст файлу та виводимо його в консоль. Не забудьте виправити назву файлу та його розширення у методі `path(forResource:ofType:)`.

## Глибше вдивіться

Щоб краще зрозуміти процес читання текстового файлу у Swift, варто розглянути його внутрішню структуру. Клас `String` насправді є структурою, яка містить декілька варіантів ініціалізації для різних типів даних, включаючи дані з файлів. Вона також використовує метод `init(contentsOfFile:usedEncoding:)` для зчитування вмісту файлу та визначення його кодування автоматично.

Крім того, на відміну від інших мов програмування, Swift використовує тип `Optional` для повернення результатів з виклику методу `String(contentsOfFile:usedEncoding:)` у випадку, якщо явно не зазначене кодування. Це дозволяє більш гнучко керувати роботою з файлами у мові Swift.

## Дивіться також

- [Робота з файлами та директоріями у Swift](https://swiftbook.ru/content/languageguide/files-and-directories/)
- [Swift Programming Language](https://developer.apple.com/library/content/documentation/Swift/Conceptual/Swift_Programming_Language/)
- [Робота зі  стрічками у Swift](https://swiftbook.ru/content/languageguide/strings/)