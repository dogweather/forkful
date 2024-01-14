---
title:                "Swift: Читання аргументів командного рядка"
simple_title:         "Читання аргументів командного рядка"
programming_language: "Swift"
category:             "Swift"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/swift/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Чому

Якщо ви програмуєте на Swift і хочете більш ефективно керувати вашою програмою, читання аргументів командного рядка є дуже корисною навичкою. Це дозволяє вам зняти значну частину вхідної інформації з вашої програми і зробити її більш динамічною.

## Як це зробити

Для читання аргументів командного рядка використовується функція `CommandLine.arguments`. Наприклад, якщо ми хочемо отримати перший аргумент, ми можемо написати:

```Swift
let firstArgument = CommandLine.arguments[1]
print("Перший аргумент: \(firstArgument)")
```

Якщо ми запустимо цей код з аргументами "hello world", то в консолі ми побачимо:

```
Перший аргумент: hello
```

## Розгон

Спочатку, коли ви побачили функцію `CommandLine.arguments`, ви можете подумати, що це просто масив вхідних аргументів. Проте, це має багато корисних функцій, таких як перевірка кількості аргументів, обробка помилок та отримання доступу до іменованих аргументів.

Наприклад, функція `CommandLine.arguments.count` дозволяє нам перевірити, чи передано аргументи у нашу програму. Якщо ви хочете передати в багато аргументів даних, ви можете використовувати `CommandLine.arguments.dropFirst()` для отримання лише аргументів.

Щоб отримати доступ до іменованих аргументів, ви можете використовувати `CommandLine.arguments.firstIndex(of: <ім'я>)` та `CommandLine.arguments[<індекс>]`. Це особливо корисно, якщо ваша програма приймає такі аргументи, як `-name "John Smith"`.

## Дивіться також

- [Документація Apple для CommandLine](https://developer.apple.com/documentation/foundation/commandline)
- [Стаття про читання аргументів командного рядка на сайті Swift by Sundell](https://www.swiftbysundell.com/basics/reading-command-line-arguments-in-swift/)