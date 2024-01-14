---
title:                "Swift: Читання аргументів командного рядка"
programming_language: "Swift"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/swift/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Чому

Записки з аргументів командного рядка можуть бути корисними для отримання вхідних даних і керування процесом програми. Це може бути особливо корисно при взаємодії з користувачем або при виконанні скриптів на сервері.

## Як

```Swift
let arguments = CommandLine.arguments
print(arguments)
```

При запуску цього коду в консолі буде виведено масив з аргументами командного рядка, які були вказані при запуску програми. Наприклад, якщо запустити програму з наступними параметрами: `./program -name John -age 25`, то в консолі буде виведено: `["./program", "-name", "John", "-age", "25"]`. Також можна використовувати ці аргументи для написання умовних виразів та полегшення роботи з даними.

## Поглиблене дослідження

Окрім звичайного зчитування аргументів командного рядка, Swift також надає можливість робити це за допомогою функцій з бібліотеки Foundation. На прикладі параметрів `--name John --age 25` це можна зробити таким чином:

```Swift
import Foundation

let arguments = ProcessInfo.processInfo.arguments
print(arguments)
```

Щоб отримати дані з аргументів командного рядка у вигляді словника, можна використовувати наступний код:

```Swift
import Foundation

let keys = CommandLine.arguments.indices.filter { $0 % 2 == 0 }.map { CommandLine.arguments[$0] }
let values = CommandLine.arguments.indices.filter { $0 % 2 == 1 }.map { CommandLine.arguments[$0] }
let arguments = Dictionary(uniqueKeysWithValues: zip(keys, values))
print(arguments)
```

Цей метод узгоджує індекси аргументів та об'єднує їх у словник, що може бути корисним для більш складних програм.

## Дивіться також

- [Документація Swift про аргументи командного рядка](https://developer.apple.com/documentation/swift/commandline/arguments)
- [Стаття "Handling Command-line Arguments in Swift 3" на сайті AppCoda](https://www.appcoda.com.tw/command-line-arguments/)
- [Відео "Working with Command-Line Arguments in Swift" з YouTube каналу Code Together](https://www.youtube.com/watch?v=2yj5GHHeRg0)