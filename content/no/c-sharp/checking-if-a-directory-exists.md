---
title:    "C#: Sjekke om en mappe eksisterer"
keywords: ["C#"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/c-sharp/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Hvorfor:

Hver gang vi utvikler software kommer vi over situasjoner der koden vår må håndtere forskjellige typer data. En vanlig operasjon er å sjekke om en katalog eksisterer. Dette kan være nyttig i tilfeller der vi må laste opp eller hente filer, eller når vi må sjekke om en bestemt katalog er tilgjengelig før vi fortsetter med prosessen vår.

## Hvordan:

Sjekking av eksistensen av en katalog er en enkel prosess i C#. Vi kan bruke klassen `Directory` for å utføre denne operasjonen.

```C#
if(Directory.Exists("min_katalog"))
{
    Console.WriteLine("Katalogen eksisterer");
}
else
{
    Console.WriteLine("Katalogen eksisterer ikke");
}
```

Koden over vil sjekke om katalogen "min_katalog" eksisterer. Hvis den gjør det vil den skrive ut en melding om at katalogen eksisterer, ellers vil den skrive ut en melding om at den ikke eksisterer.

## Deep Dive:

Hvis du trenger å utføre mer komplekse operasjoner i tillegg til å bare sjekke eksistensen av en katalog, kan du bruke metoden `GetDirectories()` fra `Directory` klassen. Denne metoden returnerer en array med navnene på alle kataloger som finnes innenfor en gitt sti. Vi kan bruke dette til å utforske flere stier og kataloger i systemet vårt.

```C#
string[] kataloger = Directory.GetDirectories("start_katalog");

foreach(string katalog in kataloger)
{
    Console.WriteLine("Katalog: " + katalog);
}
```

Koden over vil skrive ut navnene på alle kataloger som finnes i "start_katalog".

## Se også:

- [Microsoft Docs - Directory Class](https://docs.microsoft.com/nb-no/dotnet/api/system.io.directory?view=netframework-4.8)
- [C# Directory and File Exists Usage Examples](https://www.tutorialsteacher.com/csharp/csharp-check-if-file-folder-exists)