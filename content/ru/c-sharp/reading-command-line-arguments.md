---
title:                "Чтение аргументов командной строки"
date:                  2024-01-29T00:01:07.196571-07:00
model:                 gpt-4-0125-preview
simple_title:         "Чтение аргументов командной строки"
programming_language: "C#"
category:             "C#"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ru/c-sharp/reading-command-line-arguments.md"
changelog:
  - 2024-01-29, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Что и почему?
Чтение аргументов командной строки позволяет программе на C# обрабатывать пользовательский ввод, предоставленный при запуске. Программисты используют это для настройки поведения приложения без изменения кода.

## Как это сделать:
Вот как можно прочитать эти аргументы командной строки:

```C#
using System;

class Program
{
    static void Main(string[] args)
    {
        Console.WriteLine("Вы ввели следующие аргументы:");
        foreach (string arg in args)
        {
            Console.WriteLine(arg);
        }
    }
}
```

Если вы запускаете вашу программу так: `yourapp.exe arg1 arg2 arg3`, ожидайте вывод:

```
Вы ввели следующие аргументы:
arg1
arg2
arg3
```

## Подробнее
Традиция передачи аргументов командной строки ведет свое начало с рассвета вычислительной техники, позволяя раннему программному обеспечению быть гибким. В C#, `args` является массивом строк в `Main()`, содержащим переданные аргументы. Есть альтернативы? Конечно, существуют библиотеки, такие как `CommandLineParser`, которые расширяют возможности, но для многих задач `args` является вашим быстрым и грязным помощником.

Под капотом, приложение на C# начинается с `Main()`. Когда вы вызываете ваше приложение из командной строки или скрипта, операционная система помещает аргументы в массив и передает его в `Main()`. Просто и легко.

У вас сложное приложение? Возможно, вам нужно разобрать флаги, опции и значения? Вот где библиотеки светятся, предлагая больше контроля и меньше шаблонного кода, чем анализ `args` в лоб. Но для простого ввода? `args` на всем протяжении.

## Смотрите также
- [Microsoft Docs о Main() и аргументах командной строки](https://docs.microsoft.com/en-us/dotnet/csharp/fundamentals/program-structure/main-command-line)
- [Библиотека CommandLineParser на GitHub](https://github.com/commandlineparser/commandline)
- [Обсуждение на Stack Overflow о разборе аргументов командной строки в C#](https://stackoverflow.com/questions/491595/best-way-to-parse-command-line-arguments-in-c)