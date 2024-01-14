---
title:    "Swift: Читання аргументів командного рядка"
keywords: ["Swift"]
---

{{< edit_this_page >}}

## Чому?

Однією з ключових навичок програмування є робота з аргументами командного рядка, адже вони дозволяють взаємодіяти з програмою під час її виконання. Це дуже корисне та потужне інструмент, який забезпечує більш гнучке та ефективне використання програми. Тому, якщо ви хочете стати досвідченим програмістом, читання аргументів командного рядка це необхідна навичка для вас.

## Як?

Для того, щоб прочитати аргументи командного рядка у Swift, ви можете використовувати команду `CommandLine.arguments`, яка повертає масив з усіма переданими аргументами. Наприклад, якщо ви хочете виконати програму з двома аргументами "Hello" та "World", ви можете написати наступний код:

```Swift
let arguments = CommandLine.arguments
if arguments.count == 3 {
  print("Argument 1: \(arguments[1])")
  print("Argument 2: \(arguments[2])")
} else {
  print("Invalid number of arguments, please provide 2 arguments")
}
```

Після запуску програми ви повинні побачити наступний вихід:

```
Argument 1: Hello
Argument 2: World
```

## Глибші погляд

Крім зазначеної вище команди, у Swift є багато інших способів роботи з аргументами командного рядка. Наприклад, ви можете використовувати `OptionSet` для передачі ключових флагів або використовувати `CommandLine.commandName` для отримання імені програми.

Крім того, при написанні програм з аргументами командного рядка, важливо пам'ятати про перевірку на валідність та обробку помилок, оскільки використання неправильних аргументів може призвести до небажаних результатів або краху програми.

## Дивіться також

Для більш детального вивчення можливостей роботи з аргументами командного рядка у Swift, рекомендуємо ознайомитися з офіційною документацією Apple про командний рядок та аргументи.

- [Command Line Arguments - The Swift Programming Language (Swift 5.5)](https://docs.swift.org/swift-book/LanguageGuide/ControlFlow.html#ID525)
- [Using Command-Line Arguments - Apple Developer Documentation](https://developer.apple.com/documentation/swift/command-line_interfaces/using_command-line_arguments)
- [Parsing Command-Line Arguments - NSHipster](https://nshipster.com/argparse/)