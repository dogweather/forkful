---
title:                "C#: Läsning av kommandoradsargument"
simple_title:         "Läsning av kommandoradsargument"
programming_language: "C#"
category:             "C#"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/c-sharp/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

# Varför: Varför läsa inmatade argument via kommandoraden?

Att kunna läsa inmatade argument via kommandoraden är en viktig del av C# programmering. Genom att göra det kan man ge sitt program mer flexibilitet och användbarhet. Detta är speciellt viktigt när man vill kunna köra programmet med olika inmatade argument.

## Hur man gör: Exempelkod och utmatning

För att läsa inmatade argument i C# behöver man använda sig av metoden "args" som finns i "System" namespace. Här är ett enkelt exempel på hur man kan göra det:

```C#
using System;

namespace CommandLineArguments
{
    class Program
    {
        static void Main(string[] args)
        {
            // Skriv ut alla inmatade argument
            foreach (string argument in args)
            {
                Console.WriteLine(argument);
            }
        }
    }
}
```
Om vi kör detta program via kommandoraden med argumenten "hello" och "world" så skulle utmatningen vara:

```bash
hello
world
```
Detta är bara ett enkelt exempel på hur man kan använda sig av inmatade argument via kommandoraden. Det finns många andra sätt och metoder att utforska för att maximera användbarheten av ditt program.

## Djupdykning: Mer information om inmatade argument via kommandoraden

När man läser inmatade argument via kommandoraden finns det några saker man bör ha i åtanke. Först och främst, argumenten läses alltid in som strängar, så om man vill använda dem som andra datatyper måste man konvertera dem.

En annan viktig sak att tänka på är att argumenten alltid läses in i den ordning de skrivs på kommandoraden, så om ordningen är viktig för ditt program måste du se till att hantera det i din kod.

Slutligen är det viktigt att komma ihåg att argumenten alltid bör separeras med ett blanksteg på kommandoraden för att de ska läsas in korrekt.

## Se också

- [C# Dokumentation för läsa inmatade argument via kommandoraden](https://docs.microsoft.com/en-us/dotnet/csharp/programming-guide/main-and-command-args/command-line-arguments)
- [Tutorial på YouTube om läsa inmatade argument via kommandoraden i C#](https://www.youtube.com/watch?v=mT86x5-3-Jo)
- [Stack Overflow inlägg om konvertering av argument till andra datatyper](https://stackoverflow.com/questions/4978775/how-to-convert-the-command-line-arguments-to-integer-type-in-c-sharp)