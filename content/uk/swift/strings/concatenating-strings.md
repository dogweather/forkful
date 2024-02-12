---
title:                "Об'єднання рядків"
aliases:
- /uk/swift/concatenating-strings/
date:                  2024-01-20T17:35:52.823564-07:00
model:                 gpt-4-1106-preview
simple_title:         "Об'єднання рядків"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/swift/concatenating-strings.md"
---

{{< edit_this_page >}}

## Що і Чому?
Конкатенація рядків - це процес їх з'єднання в один. Програмісти конкатенують рядки, щоб створювати повідомлення, зліплювати дані, формувати динамічний контент.

## Як це робити:
```Swift
let hello = "Привіт"
let world = "Світ"
let greeting = hello + ", " + world + "!"
// Вивід: Привіт, Світ!

// Через інтерполяцію рядків
let anotherGreeting = "\(hello), \(world)!"
// Вивід: Привіт, Світ!
```

## Поглиблено:
Конкатенація рядків існує від самого народження мов програмування. У Swift, з ефективною системою рядків, конкатенація - це швидкий та простий процес. Але не завжди оптимальний при великій кількості даних, тому з’явився тип `String`, який оптимізований для змін. Для більших об'єднань краще використовувати `join`.

Альтернативи конкатенації включають використання `+`, `+=` операцій, а також інтерполяцію рядків, яку ви бачили вище. Інтерполяція зазвичай чистіша та швидша, особливо коли потрібно вставити багато змінних.

## Дивись також:
- [Swift String Interpolation](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html)
- [Swift API Reference for String](https://developer.apple.com/documentation/swift/string)
- [Using Swift’s join() method for string concatenation](https://www.hackingwithswift.com/example-code/strings/using-join-to-combine-strings)
