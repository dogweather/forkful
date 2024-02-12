---
title:                "Читання аргументів командного рядка"
aliases: - /uk/c-sharp/reading-command-line-arguments.md
date:                  2024-01-20T17:55:34.179201-07:00
model:                 gpt-4-1106-preview
simple_title:         "Читання аргументів командного рядка"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/c-sharp/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## What & Why? (Що та Чому?)
Reading command line arguments means grabbing the extra bits of data you pass to a program when you start it in a console. Programmers do it to let users tailor the program's behavior without changing the code.

## How to: (Як це зробити:)
Here's a simple code snippet for grabbing command line arguments in C#. Compile, run it via the console with some arguments, and see what happens.

```C#
using System;

class Program
{
    static void Main(string[] args)
    {
        Console.WriteLine("Arguments count: " + args.Length);
        for (int i = 0; i < args.Length; i++)
        {
            Console.WriteLine($"Argument[{i}]: {args[i]}");
        }
    }
}
```

If you run `myapp.exe arg1 arg2 arg3`, expect output like this:

```
Arguments count: 3
Argument[0]: arg1
Argument[1]: arg2
Argument[2]: arg3
```

## Deep Dive (Поглиблений Розділ):
Historically, command line arguments stem from the early days of computing when GUIs were a luxury. They're all about efficiency; why click through dialogs when a simple text command does the trick?

Alternatives exist, like reading from a config file or a database, but command line arguments are unbeatable for simplicity. Sometimes you'll use libraries like CommandLineParser for fancier argument parsing.

The `Main` method's `args` parameter is an array of strings, each element holding an argument. In .NET 5 and onward, you can also use top-level statements, making things even more succinct, though the principle remains the same.

## See Also (Дивіться також):
- [Official Microsoft Docs on Command-Line Args](https://docs.microsoft.com/en-us/dotnet/csharp/programming-guide/main-and-command-args/)
- [.NET Core CommandLineParser Library](https://github.com/commandlineparser/commandline)
- [Stack Overflow: How to parse command line arguments](https://stackoverflow.com/questions/491595/best-way-to-parse-command-line-arguments-in-c)
