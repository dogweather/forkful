---
date: 2024-01-20 17:55:34.368422-07:00
description: "How to: (Pog\u0142\u0119biona wiedza) Argumenty linii komend by\u0142\
  y wykorzystywane od czas\xF3w pierwszych komputer\xF3w. Pozwala\u0142y one na proste\
  \ i szybkie przekazywanie\u2026"
lastmod: '2024-04-05T22:50:49.744522-06:00'
model: gpt-4-1106-preview
summary: "(Pog\u0142\u0119biona wiedza) Argumenty linii komend by\u0142y wykorzystywane\
  \ od czas\xF3w pierwszych komputer\xF3w."
title: "Odczytywanie argument\xF3w linii polece\u0144"
weight: 23
---

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
