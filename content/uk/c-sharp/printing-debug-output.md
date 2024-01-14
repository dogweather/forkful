---
title:                "C#: Виведення дебаг-виводу"
programming_language: "C#"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/c-sharp/printing-debug-output.md"
---

{{< edit_this_page >}}

## Чому

В програмуванні важливо використовувати відлагодження для знаходження та виправлення помилок у коді. Вибірково виводити результати тестів та змінні через print-команду допомагає швидше і ефективніше встановлювати проблемні місця в програмі.

## Як

```c#
// Приклад коду для виводу відлагодження в C#
int number1 = 5;
int number2 = 10;

// Вивести значення number1 та number2
Console.WriteLine($"Перше число: {number1}");
Console.WriteLine($"Друге число: {number2}");

// Виводимо суму обох чисел
Console.WriteLine($"Сума: {number1 + number2}");

/* Виводиться результат:
Перше число: 5
Друге число: 10
Сума: 15
*/

```

## Глибинний аналіз

Іноді виводити відлагодження необхідно в більш складних структурах даних, наприклад, в масивах або об'єктах. В таких випадках, корисно використовувати цикли та методи для виведення даних із цих структур. Також важливо правильно форматувати виведений текст, щоб було зручно читати та аналізувати. 

## Дивись також

- [Microsoft документація про Console.WriteLine](https://docs.microsoft.com/en-us/dotnet/api/system.console.writeline?view=net-5.0)
- [Відео з поясненням про використання відлагодження в C#](https://www.youtube.com/watch?v=-gd6Ry2s3JI)
- [Туторіал про відлагодження в C#](https://www.tutorialspoint.com/csharp/csharp_debugging.htm)