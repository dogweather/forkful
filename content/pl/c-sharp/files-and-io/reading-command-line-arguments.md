---
title:                "Odczytywanie argumentów linii poleceń"
aliases: - /pl/c-sharp/reading-command-line-arguments.md
date:                  2024-01-20T17:55:34.368422-07:00
model:                 gpt-4-1106-preview
simple_title:         "Odczytywanie argumentów linii poleceń"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/c-sharp/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## What & Why?
(Co i Dlaczego?)

Czytanie argumentów linii komend to sposób, by nasz program C# mógł otrzymywać dane bezpośrednio od użytkownika za pomocą terminala. Programiści używają tej metody, by zwiększyć elastyczność i interaktywność programów, także by ułatwić automatyzację zadań i przetwarzanie wsadowe.

## How to:
(Jak to zrobić?)

```C#
// Przykład prostego programu, który czyta argumenty z linii komend

using System;

class CommandLineArgs
{
    static void Main(string[] args)
    {
        Console.WriteLine("Liczba otrzymanych argumentów: " + args.Length);
        for (int i = 0; i < args.Length; i++)
        {
            Console.WriteLine($"Argument {i+1}: {args[i]}");
        }
    }
}

// Przykładowe wywołanie programu:
// dotnet run -- arg1 arg2 arg3

// Oczekiwane wyjście (output):
// Liczba otrzymanych argumentów: 3
// Argument 1: arg1
// Argument 2: arg2
// Argument 3: arg3
```

## Deep Dive:
(Pogłębiona wiedza)

Argumenty linii komend były wykorzystywane od czasów pierwszych komputerów. Pozwalały one na proste i szybkie przekazywanie informacji do programów. W C# argumenty te są przechowywane w tablicy `string[] args`, która jest parametrem metody `Main`.

Alternatywy? Możemy używać też pliki konfiguracyjne, zmienne środowiskowe, czy interakcje z bazą danych. Jednak argumenty linii komend są z reguły szybsze i prostsze do jednorazowych działań.

Szczegóły implementacyjne? `args` może być puste, jeśli nie przekażemy żadnych argumentów. Ważne jest by pamiętać o walidacji danych wejściowych, by nasz program był odporny na błędy i nieprawidłowe użycie.

## See Also:
(Zobacz również)

- [Microsoft Docs - Main() and command-line arguments](https://docs.microsoft.com/en-us/dotnet/csharp/programming-guide/main-and-command-args/)
- [Microsoft Docs - Command-line parameters](https://docs.microsoft.com/en-us/dotnet/api/system.environment.getcommandlineargs?view=net-6.0)
- [Stack Overflow - How to handle command line arguments in C#](https://stackoverflow.com/questions/491595/best-way-to-parse-command-line-arguments-in-c)
