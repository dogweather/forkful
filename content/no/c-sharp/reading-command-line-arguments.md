---
title:    "C#: Lesing av kommandolinjeargumenter"
keywords: ["C#"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/c-sharp/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Hvorfor

Å lese kommandolinjeargumenter kan være en nyttig ferdighet å ha i C#-programmering. Det kan hjelpe deg med å lage mer fleksible og interaktive programmer, som kan være spesielt nyttig i konsoll-applikasjoner. Ved å kunne lese argumenter fra kommandolinjen, kan du også gi mer kontroll og tilpasning til brukeren av ditt program.

## Hvordan

Først må du definere din Main-metode med en `string[] args` parameter. Dette gjør at programmet ditt kan ta imot kommandolinjeargumenter når det blir startet. Deretter kan du bruke `args` arrayet til å lese argumentene og ta handling basert på dem.

For å demonstrere dette, la oss lage et enkelt program som sjekker om den første kommandolinje-argumentet er et tall eller en tekststreng. Vi kan gjøre dette ved å bruke `TryParse`-metoden til `int` og `string`-klassene. Se koden under for et eksempel:

```C#
using System;

public class Program
{
    public static void Main (string[] args)
    {
        if (int.TryParse(args[0], out int num))
        {
            Console.WriteLine($"{num} er et tall.");
        }
        else
        {
            Console.WriteLine($"{args[0]} er en tekststreng.");
        }
    }
}
```

Hvis du kjører dette programmet med forskjellige argumenter, vil du se forskjellige resultater. For eksempel `program.exe 123` vil gi følgende output: `123 er et tall.`, mens `program.exe abc` vil gi: `abc er en tekststreng.`

## Dypdykk

I eksempelet ovenfor brukte vi `args`-arrayet og `TryParse`-metoden for å sjekke om det første argumentet er et tall. Men det er også andre nyttige metoder og egenskaper som kan brukes til å lese og håndtere kommandolinjeargumenter, som f.eks. `Environment.GetCommandLineArgs()`, `Environment.CommandLine` og `Environment.CurrentDirectory`.

Det er også mulig å sende inn kommandolinjeargumenter til et C#-program når du kjører det fra terminalen eller kommandolinjen. Dette kan gjøres ved å skrive programnavnet etterfulgt av argumentene dine, for eksempel: `program.exe arg1 arg2 arg3`.

Å lese og håndtere kommandolinjeargumenter kan øke funksjonaliteten og brukervennligheten til programmene dine. Det kan også være en nyttig ferdighet å ha når du jobber med konsoll-applikasjoner eller trenger å gi mer kontroll til brukerne dine.

## Se også

- [Microsoft Dokumentasjon om Environment.GetCommandLineArgs()](https://docs.microsoft.com/en-us/dotnet/api/system.environment.getcommandlineargs?view=netcore-3.1)
- [C# Tutorial: Kommandolinjeargumenter](https://www.tutorialspoint.com/csharp/csharp_command_line_arguments.htm)
- [Kommandolinjeargumenter i C#](https://www.c-sharpcorner.com/article/command-line-arguments-in-c-sharp/)