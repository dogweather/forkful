---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:05:06.952604-07:00
description: 'Hoe: Zo verslind je die commandoregelargumenten.'
lastmod: '2024-03-13T22:44:50.825463-06:00'
model: gpt-4-0125-preview
summary: Zo verslind je die commandoregelargumenten.
title: Commandoregelargumenten lezen
weight: 23
---

## Hoe:
Zo verslind je die commandoregelargumenten:

```C#
using System;

class Program
{
    static void Main(string[] args)
    {
        Console.WriteLine("Je hebt de volgende argumenten ingevoerd:");
        foreach (string arg in args)
        {
            Console.WriteLine(arg);
        }
    }
}
```

Als je je programma zo uitvoert: `jouwapp.exe arg1 arg2 arg3`, verwacht dan de uitvoer:

```
Je hebt de volgende argumenten ingevoerd:
arg1
arg2
arg3
```

## Diepe Duik
De traditie van commandoregelargumenten gaat terug tot het begin van de computertijd, waardoor vroege software flexibel kon zijn. In C# is `args` een string-array in `Main()` die de doorgegeven argumenten bevat. Alternatieven? Zeker, er zijn bibliotheken zoals `CommandLineParser` die de mogelijkheden versterken, maar voor veel taken is `args` je snelle en vieze vriend.

Onder de motorkap begint een C# app met `Main()`. Wanneer je je app vanaf een commandoregel of script aanroept, stopt het besturingssysteem de argumenten in een array en geeft deze door aan `Main()`. Makkelijk zat.

Heb je een complexe app? Misschien heb je het nodig om vlaggen, opties en waarden te parsen? Dat is waar bibliotheken schitteren met meer controle en minder boilerplate-code dan ruwe `args`-parsing. Maar voor eenvoudige invoer? `args` helemaal.

## Zie Ook
- [Microsoft Docs over Main() en commandoregelargumenten](https://docs.microsoft.com/nl-nl/dotnet/csharp/fundamentals/program-structure/main-command-line)
- [CommandLineParser-bibliotheek op GitHub](https://github.com/commandlineparser/commandline)
- [Stack Overflow-discussie over het parsen van commandoregelargumenten in C#](https://stackoverflow.com/questions/491595/best-way-to-parse-command-line-arguments-in-c)
