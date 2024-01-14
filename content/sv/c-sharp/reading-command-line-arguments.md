---
title:                "C#: Läsning av kommandoradsargument"
programming_language: "C#"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/c-sharp/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Varför

Att läsa kommandoradsargument är en viktig del av programmering eftersom det tillåter oss att skicka och ta emot information från våra program via terminalen. Detta gör det möjligt för användare att anpassa hur programmet körs och ger en mer dynamisk upplevelse.

## Hur man gör det

För att läsa kommandoradsargument i C# kan vi använda oss av klassen `args` som innehåller alla argument som skickas till programmet via terminalen. Vi kan sedan loopa igenom dessa argument och hämta informationen som vi behöver. Här är ett enkelt exempel på hur man kan göra det:

```C#
class Program
{
    static void Main(string[] args)
    {
        // Loopar igenom alla kommandoradsargument
        for (int i = 0; i < args.Length; i++)
        {
            Console.WriteLine($"Argument {i+1}: {args[i]}");
        }
    }
}
```

Om vi nu kör detta program med argumenten `dotnet run arg1 arg2 arg3`, kommer följande att skrivas ut:

```
Argument 1: arg1
Argument 2: arg2
Argument 3: arg3
```

Detta visar hur vi enkelt kan hämta information från kommandoradsargument och använda den i vårt program. Vi kan sedan använda oss av villkor och kontrollstrukturer för att göra vårt program mer flexibelt och anpassningsbart.

## Djupdykning

När vi läser kommandoradsargument är det viktigt att tänka på säkerheten i vårt program. Vi bör alltid validera och filtrera användar-input för att undvika eventuella säkerhetsproblem. Detta kan göras genom att till exempel bara acceptera specifika typer av argument eller kontrollera att argumenten följer ett visst format.

Vi kan också använda oss av externa bibliotek för att göra vår kod mer robust och hantera mer komplexa kommandoradsargument. Till exempel kan vi använda "CommandLineParser" som ger oss möjlighet att hantera olika typer av argument och lägga till egna anpassade funktioner.

## Se också

- [Microsoft Docs - Läsa kommandoradsargument i .NET](https://docs.microsoft.com/sv-se/dotnet/csharp/programming-guide/main-and-command-args/command-line-arguments)
- [CommandLineParser NuGet-paket](https://www.nuget.org/packages/CommandLineParser/)