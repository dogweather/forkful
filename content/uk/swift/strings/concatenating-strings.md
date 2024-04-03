---
date: 2024-01-20 17:35:52.823564-07:00
description: "\u042F\u043A \u0446\u0435 \u0440\u043E\u0431\u0438\u0442\u0438: ."
lastmod: '2024-03-13T22:44:49.905667-06:00'
model: gpt-4-1106-preview
summary: .
title: "\u041E\u0431'\u0454\u0434\u043D\u0430\u043D\u043D\u044F \u0440\u044F\u0434\
  \u043A\u0456\u0432"
weight: 3
---

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
