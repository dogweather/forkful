---
title:                "Читання аргументів командного рядка"
html_title:           "Arduino: Читання аргументів командного рядка"
simple_title:         "Читання аргументів командного рядка"
programming_language: "Swift"
category:             "Swift"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/swift/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Що та навіщо?

Читання аргументів командного рядка - це процес введення параметрів, що контролює поведінку програми. Програмісти роблять це, щоб програма могла бути більш гнучкою та кастомізованою.

## Як це зробити:

В Swift аргументи командного рядка можна зчитати дуже легко. Використовуйте цей код:
```Swift
let arguments = CommandLine.arguments
print(arguments)
```
При запуску програми з аргументами, наприклад `./myProgram arg1 arg2`, виведе: `["./myProgram", "arg1", "arg2"]`.

## Пірнемо глибше:

З переходом до MacOS X, Unix-подібний Swift прийняв `argc` та `argv` для читання аргументів командного рядка. Існують альтернативи, такі як бібліотека `CommandLineKit` для більш складних потреб, але стандартний підхід Swift простий та ефективний. Ці аргументи передаються операційній системі при виконанні програми, а потім Swift читає їх з `CommandLine.arguments`.

## Дивись також:

- [Swift Programming: The Big Nerd Ranch Guide](https://www.bignerdranch.com/books/swift-programming/)
- [Apple's Swift Documentation](https://developer.apple.com/documentation/swift)
- [CommandLineKit on GitHub](https://github.com/jatoben/CommandLine)