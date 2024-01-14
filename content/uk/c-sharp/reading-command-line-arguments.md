---
title:                "C#: Читання аргументів командного рядка"
programming_language: "C#"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/c-sharp/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Чому

При програмуванні в C# важливо знати, як правильно прочитати аргументи командного рядка. Це дозволяє передати в програму додаткові дані з використанням командного рядка, що є особливо корисним для написання затребуваних та відсторонених додатків.

## Як це зробити

Розглянемо приклад нижче, де ми прочитаємо аргументи командного рядка і виведемо їх на екран:

```C#
using System;

class Program 
{
    static void Main(string[] args) 
    {
        foreach (string arg in args) 
        {
            Console.WriteLine(arg);
        }
    }
}

```

Викликаючи цю програму з наступними аргументами командного рядка:

```
> program.exe arg1 arg2 arg3
```

ми отримаємо наступний результат:

```
arg1
arg2
arg3
```

Щоб зробити цей приклад ще кориснішим, ви можете також використовувати аргументи командного рядка для передачі значень змінним в програмі, наприклад:

```C#
using System;

class Program 
{
    static void Main(string[] args) 
    {
        string name = args[0];
        int age = int.Parse(args[1]);
        Console.WriteLine($"{name} is {age} years old.");
    }
}

```

Цей код визначає змінні за допомогою значень аргументів командного рядка і виводить рядок, що містить ці змінні, на екран. Приклад запуску програми з аргументами:

```
> program.exe John 25
```

виведе наступне:

```
John is 25 years old.
```

## Глибше

Крім того, можна використовувати більш складні методи для парсінгу аргументів командного рядка, такі як використання бібліотеки `CommandLineParser` з NuGet, щоб отримати доступ до більш гнучкого та зручного способу отримання та обробки аргументів.

## Дивіться також

- [Мануал з командного рядка C#](https://docs.microsoft.com/en-us/dotnet/csharp/programming-guide/main-and-command-args/)
- [Бібліотека CommandLineParser для роботи з аргументами командного рядка в C#](https://github.com/commandlineparser/commandline)