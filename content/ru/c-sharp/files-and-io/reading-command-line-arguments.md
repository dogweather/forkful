---
changelog:
- 2024-01-29, gpt-4-0125-preview, translated from English
date: 2024-01-29 00:01:07.196571-07:00
description: "\u0427\u0442\u0435\u043D\u0438\u0435 \u0430\u0440\u0433\u0443\u043C\u0435\
  \u043D\u0442\u043E\u0432 \u043A\u043E\u043C\u0430\u043D\u0434\u043D\u043E\u0439\
  \ \u0441\u0442\u0440\u043E\u043A\u0438 \u043F\u043E\u0437\u0432\u043E\u043B\u044F\
  \u0435\u0442 \u043F\u0440\u043E\u0433\u0440\u0430\u043C\u043C\u0435 \u043D\u0430\
  \ C# \u043E\u0431\u0440\u0430\u0431\u0430\u0442\u044B\u0432\u0430\u0442\u044C \u043F\
  \u043E\u043B\u044C\u0437\u043E\u0432\u0430\u0442\u0435\u043B\u044C\u0441\u043A\u0438\
  \u0439 \u0432\u0432\u043E\u0434, \u043F\u0440\u0435\u0434\u043E\u0441\u0442\u0430\
  \u0432\u043B\u0435\u043D\u043D\u044B\u0439 \u043F\u0440\u0438 \u0437\u0430\u043F\
  \u0443\u0441\u043A\u0435. \u041F\u0440\u043E\u0433\u0440\u0430\u043C\u043C\u0438\
  \u0441\u0442\u044B \u0438\u0441\u043F\u043E\u043B\u044C\u0437\u0443\u044E\u0442\
  \ \u044D\u0442\u043E\u2026"
lastmod: '2024-03-13T22:44:45.084885-06:00'
model: gpt-4-0125-preview
summary: "\u0427\u0442\u0435\u043D\u0438\u0435 \u0430\u0440\u0433\u0443\u043C\u0435\
  \u043D\u0442\u043E\u0432 \u043A\u043E\u043C\u0430\u043D\u0434\u043D\u043E\u0439\
  \ \u0441\u0442\u0440\u043E\u043A\u0438 \u043F\u043E\u0437\u0432\u043E\u043B\u044F\
  \u0435\u0442 \u043F\u0440\u043E\u0433\u0440\u0430\u043C\u043C\u0435 \u043D\u0430\
  \ C# \u043E\u0431\u0440\u0430\u0431\u0430\u0442\u044B\u0432\u0430\u0442\u044C \u043F\
  \u043E\u043B\u044C\u0437\u043E\u0432\u0430\u0442\u0435\u043B\u044C\u0441\u043A\u0438\
  \u0439 \u0432\u0432\u043E\u0434, \u043F\u0440\u0435\u0434\u043E\u0441\u0442\u0430\
  \u0432\u043B\u0435\u043D\u043D\u044B\u0439 \u043F\u0440\u0438 \u0437\u0430\u043F\
  \u0443\u0441\u043A\u0435. \u041F\u0440\u043E\u0433\u0440\u0430\u043C\u043C\u0438\
  \u0441\u0442\u044B \u0438\u0441\u043F\u043E\u043B\u044C\u0437\u0443\u044E\u0442\
  \ \u044D\u0442\u043E\u2026"
title: "\u0427\u0442\u0435\u043D\u0438\u0435 \u0430\u0440\u0433\u0443\u043C\u0435\u043D\
  \u0442\u043E\u0432 \u043A\u043E\u043C\u0430\u043D\u0434\u043D\u043E\u0439 \u0441\
  \u0442\u0440\u043E\u043A\u0438"
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
