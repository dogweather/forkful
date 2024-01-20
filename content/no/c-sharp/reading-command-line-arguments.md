---
title:                "Lese kommandolinjeargumenter"
html_title:           "Arduino: Lese kommandolinjeargumenter"
simple_title:         "Lese kommandolinjeargumenter"
programming_language: "C#"
category:             "C#"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/c-sharp/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Hva og hvorfor?

Kommandolinjeargumenter er parametere som sendes til programmet ved oppstart. Disse lar en bruker påvirke hvordan programmet fungerer uten å endre koden, noe som er spesielt hensiktsmessig for programmer med varierte funksjoner og innstillinger.

## Hvordan:

Reglene er enkle. I C# mottas kommandolinjeargumenter gjennom et argument til Main()-funksjonen, vanligvis kalt 'args'. La oss gjøre et eksempel:

```C# 
static void Main(string[] args)
{
    for (int i = 0; i < args.Length; i++)
    {
        Console.WriteLine($"Argument {i}: {args[i]}");
    }
}
```

I dette eksempelet vil programmet skrive ut hvert kommandolinjeargument på en ny linje. Hvis du kjører programmet med argumentene 'Hello', 'World', vil du se:

``` 
Argument 0: Hello
Argument 1: World
```

## Dypere dykk:

Historien til kommandolinjeargumenter strekker seg tilbake til de eldste dagene for programmering. Selv om moderne grafiske grensesnitt nå dominerer, er kommandolinjeparametere fortsatt et kraftig verktøy.

Alternativt, hvis du trenger en mer avansert behandling av kommandolinjeargumenter som flagg eller nøkkel/verdi-par, kan du vurdere å bruke et bibliotek som `CommandLineParser`.

Implementeringsdetaljer er ganske rett frem. Kommandolinjeargumenter sendes som en serie strenger, der hvert argument er adskilt av et mellomrom. Hvis et argument skal inneholde et mellomrom, må det omsluttes med anførselstegn.

## Se også:

* Microsoft C# Programmering Guide: https://docs.microsoft.com/en-us/dotnet/csharp/
* CommandLineParser bibliotek: https://github.com/commandlineparser/commandline