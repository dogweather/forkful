---
title:                "Lese kommandolinjeargumenter"
aliases:
- no/c-sharp/reading-command-line-arguments.md
date:                  2024-01-20T17:55:42.021560-07:00
model:                 gpt-4-1106-preview
simple_title:         "Lese kommandolinjeargumenter"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/c-sharp/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Kommandolinjeargumenter lar oss sende informasjon til et program når vi starter det. Vi gjør dette for å gi fleksibel kontroll over programmets oppførsel uten å hardkode verdier.

## Hvordan:
```C#
using System;

class CommandLineDemo
{
    static void Main(string[] args)
    {
        Console.WriteLine("Antall argumenter: " + args.Length);
        for (int i = 0; i < args.Length; i++)
        {
            Console.WriteLine($"Argument {i + 1}: {args[i]}");
        }
    }
}

```
Kjør programmet slik:
```
dotnet run -- arg1 arg2 arg3
```
Forventet utskrift:
```
Antall argumenter: 3
Argument 1: arg1
Argument 2: arg2
Argument 3: arg3
```

## Dypdykk
Før i tiden, da GUIer var sjeldne, var kommandolinjeinteraksjon essensiell. Det er fortsatt kritisk for skripting, automatisering og bruken i utviklingsverktøy. Alternativer til kommandolinjeargumenter inkluderer konfigurasjonsfiler og interaktive prompts, men de er mindre effektive for rutineoppgaver. Implementeringsdetaljer varierer mellom operativsystemer; i Windows brukes for eksempel `Main(string[] args)` mens i UNIX-baserte systemer bruker man `int main(int argc, char* argv[])`.

## Se Også
- [Microsoft Docs: Command-line parameters](https://docs.microsoft.com/en-us/dotnet/csharp/programming-guide/main-and-command-args/)
- [Stack Overflow: How to parse command line arguments](https://stackoverflow.com/questions/491595/best-way-to-parse-command-line-arguments-in-c)
- [Wikipedia: Command-line interface](https://en.wikipedia.org/wiki/Command-line_interface)
