---
title:                "Å starte et nytt prosjekt."
html_title:           "C#: Å starte et nytt prosjekt."
simple_title:         "Å starte et nytt prosjekt."
programming_language: "C#"
category:             "C#"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/c-sharp/starting-a-new-project.md"
---

{{< edit_this_page >}}

## Hvorfor
Det å starte et nytt program kan være en spennende og givende opplevelse. Det gir deg muligheten til å skape noe helt nytt og utforske dine programmeringsevner. Det er også en flott måte å lære nye teknologier og forbedre dine ferdigheter på.

## Hvordan
For å starte et nytt C# prosjekt, åpner du Visual Studio og følger disse enkle trinnene:

1. I startmenyen velger du “Create a new project” (Lag et nytt prosjekt).
2. Velg C# som språk og velg et passende prosjektnavn.
3. Velg en plassering for prosjektet ditt og velg “Create” (Lag).
4. Nå er du klar til å begynne å kode! Du kan begynne med å skrive koden din i main-funksjonen i filen Program.cs, som vil bli kjørt når du starter programmet.

Her er et eksempel på en liten “Hello World” applikasjon i C#:

```C#
using System;

class Program
{
    static void Main(string[] args)
    {
        Console.WriteLine("Hello World!");
        Console.ReadLine();
    }
}
```

Dette vil skrive ut teksten “Hello World!” og vente på at brukeren skal trykke en tast før programmet avsluttes.

## Dypdykk
Når du starter et nytt prosjekt i C#, vil det generere en del filer og mapper for deg. Her er en kort forklaring på noen av de viktigste filene og mappene:

- Program.cs: Dette er hovedfilen til prosjektet ditt, der startpunktet for programmet er.
- bin og obj mapper: Disse mappene inneholder kompilerte filer og midlertidige filer som brukes under byggeprosessen.
- Packages.config: Denne filen inneholder informasjon om hvilke pakker som er installert i prosjektet ditt fra NuGet, en populær pakkebehandlingstjeneste for .NET.
- Properties mappe: Denne mappen inneholder en AssemblyInfo.cs fil som inneholder informasjon om prosjektet ditt, som for eksempel versjonsnummer og selskapsnavn.

Du kan også velge å legge til ekstra mapper og filer for å organisere koden din på en mer effektiv måte. Det er også viktig å merke seg at når du bygger og kjører prosjektet, vil det generere en .exe-fil som du kan dele med andre for å kjøre programmet ditt.

## Se Også
- [C# Programmering: Fra Nybegynner til Ekspert](https://www.c-sharpcorner.com/article/c-sharp-programming/)
- [Offisiell C# Dokumentasjon](https://docs.microsoft.com/en-us/dotnet/csharp/)
- [NuGet](https://www.nuget.org/) for å finne og installere pakker til ditt C# prosjekt.