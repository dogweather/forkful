---
title:    "Swift: Читання аргументів командного рядка"
keywords: ["Swift"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/uk/swift/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Почему

Командная строка – это важная часть программирования Swift. Чтение аргументов командной строки позволяет вам взаимодействовать с программой во время ее выполнения. В этом блоге мы рассмотрим, как читать аргументы командной строки в своих программах на Swift.

## Как это сделать

Для чтения аргументов командной строки в Swift, мы можем использовать класс CommandLine. Давайте посмотрим на простой пример кода, который печатает аргументы командной строки:

```Swift
let arguments = CommandLine.arguments
for argument in arguments {
    print(argument)
}
```

Если мы запустим этот код с аргументами "Hello" и "World", то увидим следующий вывод:

```
/path/to/your/program
Hello
World
```

## Глубокий погружения

Класс CommandLine имеет много полезных свойств и методов, которые могут помочь нам работать с аргументами командной строки. Например, мы можем получить имя самой программы с помощью свойства `CommandLine.arguments.first` или обработать аргументы после определенного индекса с помощью метода `CommandLine.arguments.dropFirst()`. Для более подробной информации, вы можете обратиться к документации по классу `CommandLine`.

## Смотрите также

- [Документация по классу CommandLine](https://developer.apple.com/documentation/foundation/commandline)
- [Статья о работе с командной строкой в Swift](https://www.raywenderlich.com/5123-command-line-programs-on-macos-tutorial)