---
title:                "Читання аргументів командного рядка"
html_title:           "Arduino: Читання аргументів командного рядка"
simple_title:         "Читання аргументів командного рядка"
programming_language: "C#"
category:             "C#"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/c-sharp/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Що і чому?

Считування аргументів командного рядка це процес, де програма отримує дані прямо при старті з командного рядка. Програмісти це роблять для підготовки програмного забезпечення до гнучкого використання, щоб налаштувати його поведінку або вхідні дані.

## Як це робити:

```C#
static void Main(string[] args)
{
    Console.WriteLine("Arg Count: " + args.Length);
    for (int i = 0; i < args.Length; i++)
    {
        Console.WriteLine("Arg[" + i + "]: " + args[i]);
    }
}
```

Якщо ви спробуєте запустити цю програму з командного рядка (наприклад, `program.exe arg1 arg2 arg3`), вона виведе:

```C#
Arg Count: 3
Arg[0]: arg1
Arg[1]: arg2
Arg[2]: arg3
```

## Поглиблений погляд

Спочатку, считування аргументів командного рядка було основним способом, щоб програма могла взаємодіяти з користувачем або іншими програмами. Цей метод використовувався в Unix-like системах.

Як альтернативу, можна використовувати конфгураційні файли або GUI, але вони не надають такої гнучкості для автоматизації.

У C#, аргументи командного рядка передаються в метод `Main()` як масив рядків, де кожний елемент масиву - це віддільний аргумент.

## Скористайтесь також

1. Документація Microsoft з аргументами командного рядка: [https://docs.microsoft.com/en-us/dotnet/csharp/programming-guide/main-and-command-args/](https://docs.microsoft.com/en-us/dotnet/csharp/programming-guide/main-and-command-args/)