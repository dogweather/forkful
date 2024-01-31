---
title:                "Читання аргументів командного рядка"
date:                  2024-01-20T17:57:38.772371-07:00
model:                 gpt-4-1106-preview
simple_title:         "Читання аргументів командного рядка"

category:             "Swift"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/swift/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Що це таке та навіщо?
Читання аргументів командного рядка - це процес отримання даних, які користувач передав у вашу програму. Програмісти роблять це, щоб зробити свої програми гнучкішими та налаштовуваними.

## Як це зробити:
Swift надає доступ до аргументів командного рядка через глобальний масив `CommandLine.arguments`. Ось приклад того, як прочитати і використати ці аргументи:

```Swift
// Надрукуємо всі аргументи командного рядка
for argument in CommandLine.arguments {
    print(argument)
}

// Використання аргумента як частини інструкції
if CommandLine.arguments.contains("--verbose") {
    print("Verbose mode is on.")
}

// Робота з опціональними значеннями для аргументів, що містяться в парах key=value
let arguments = CommandLine.arguments.reduce(into: [String: String]()) { result, argument in
    let keyValue = argument.split(separator: "=").map(String.init)
    if keyValue.count == 2 {
        result[keyValue[0]] = keyValue[1]
    }
}

if let path = arguments["--file"] {
    print("File path provided: \(path)")
}
```

Якщо запустити програму з аргументом `--file=example.txt`, наше маленьке додаток виведе:
```
File path provided: example.txt
```

## Поглиблено:
Історично, читання аргументів командного рядка було одним з основних способів взаємодії з користувацькими програмами. Це дає користувачам можливість запускати скрипти із заздалегідь визначеними налаштуваннями, автоматизувати завдання та інтегруватися з іншими інструментами. 

У Swift альтернатив не так багато, як у більш старих мовах програмування, але можна використовувати сторонні бібліотеки, які надають більш складну обробку аргументів командного рядка, такі як `SwiftArgumentParser`. Вони можуть обробляти флаги, опції та надавати автоматично генеровані довідки.

Деталі імплементації зчитування аргументів зводяться до роботи з масивом рядків, які подані системою як вхідні дані. У реальних програмах важливо робити перевірку на помилки і переконатись, що аргументи, які користувач передає, є валідними.

## Дивіться також:
Скористайтеся цими ресурсами для більш глибокого занурення у тему:

- Бібліотека SwiftArgumentParser на [GitHub](https://github.com/apple/swift-argument-parser)
- Розширений гайд по обробці командного рядка в Swift на [raywenderlich.com](https://www.raywenderlich.com/511-command-line-programs-on-macos-tutorial)
