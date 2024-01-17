---
title:                "Читання аргументів командного рядка"
html_title:           "C#: Читання аргументів командного рядка"
simple_title:         "Читання аргументів командного рядка"
programming_language: "C#"
category:             "C#"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/c-sharp/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

Що і чому?

У програмуванні існує поняття "читання аргументів командного рядка". Це означає отримання інформації, що вказана під час запуску програми в командному рядку. Програмісти читають аргументи командного рядка для отримання додаткової інформації або налаштувань для своїх програм.

Як це зробити?

Нижче наведені приклади коду та виводу для читання аргументів командного рядка в C#.

```C#
using System;

namespace CommandLineArguments
{
    class Program
    {
        static void Main(string[] args)
        {
            // Приклад читання першого аргумента командного рядка
            string firstArgument = args[0];
            Console.WriteLine("Перший аргумент: " + firstArgument);

            // Приклад читання всіх аргументів командного рядка
            foreach (string arg in args)
            {
                Console.WriteLine("Аргумент: " + arg);
            }
        }
    }
}
```

Вивід:
```
Перший аргумент: Вітаю!
Аргумент: Вітаю!
Аргумент: Це
Аргумент: код
Аргумент: для
Аргумент: виводу
Аргумент: аргументів
Аргумент: командного
Аргумент: рядка
```

Детальніше про те, як читати аргументи командного рядка

Читання аргументів командного рядка було довго часу єдиним способом передачі параметрів у програму. Проте з появою графічного інтерфейсу користувача, зростанням популярності веб-додатків та API, цей підхід втратив свою популярність. Існують інші способи передачі параметрів, такі як файл конфігурації або використання зовнішніх бібліотек. Якщо Ви все ж хочете читати аргументи командного рядка, ми радимо докладніше ознайомитися з класом `CommandLine` з простору імен `System.CommandLine` або розглянути використання готових зовнішніх бібліотек, наприклад `CommandLineParser`.

Подивитися також:

- Microsoft Docs: [Читання аргументів командного рядка в C#](https://docs.microsoft.com/uk-ua/dotnet/csharp/programming-guide/main-and-command-args/command-line-arguments)
- Microsoft Docs: [Клас `CommandLine`](https://docs.microsoft.com/uk-ua/dotnet/api/system.commandline.commandline)
- NuGet: [CommandLineParser](https://www.nuget.org/packages/CommandLineParser/)