---
title:                "Een tijdelijk bestand aanmaken"
date:                  2024-01-28T21:58:16.929377-07:00
model:                 gpt-4-0125-preview
simple_title:         "Een tijdelijk bestand aanmaken"
programming_language: "C#"
category:             "C#"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/c-sharp/creating-a-temporary-file.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?
Een tijdelijk bestand aanmaken betekent dat je een bestand genereert voor een kortetermijndoel, zoals het opslaan van gegevens die alleen nodig zijn tijdens de uitvoering van een programma. Programmeurs doen dit om verschillende redenen, zoals het vermijden van geheugenbeperkingen of het bewaren van een tijdelijke staat tijdens complexe bewerkingen.

## Hoe te:
Hier is een snelle manier om een tijdelijk bestand te creëren en er naar te schrijven in C#:

```C#
using System;
using System.IO;

class Program
{
    static void Main()
    {
        // Maak een tijdelijk bestand aan
        string tempFilePath = Path.GetTempFileName();

        // Schrijf iets naar het tijdelijke bestand
        File.WriteAllText(tempFilePath, "Hallo, Tijdelijke Wereld!");

        // Lees terug uit het tijdelijke bestand en print
        string fileContents = File.ReadAllText(tempFilePath);
        Console.WriteLine(fileContents);

        // Ruim het tijdelijke bestand op
        File.Delete(tempFilePath);
    }
}
```

Voorbeelduitvoer:
```
Hallo, Tijdelijke Wereld!
```

## Diepere Duik
Tijdelijke bestanden bestaan al sinds de vroege dagen van de informatica, toen het minimaliseren van geheugengebruik cruciaal was. Ze bieden een sandbox-omgeving voor programma's om met gegevens te werken zonder de gevolgen van langdurige opslag.

Naast `Path.GetTempFileName()`, heb je andere keuzes zoals `Path.GetRandomFileName()`, die geen bestand creëert maar je een naam geeft om te gebruiken voor een tijdelijk bestand. Ook kan de `System.IO.TempFileCollection`-klasse meerdere tijdelijke bestanden beheren, handig wanneer je er meer dan één nodig hebt.

Onder de motorkap maken de meeste C#-methoden voor het aanmaken van tijdelijke bestanden gebruik van API's die door het onderliggende besturingssysteem worden aangeboden. In Windows koppelt `GetTempFileName()` aan een vergelijkbare Win32 API-functie die de uniciteit van de bestandsnaam garandeert en het beveiligt tegen botsingen.

Zorg ervoor dat je altijd tijdelijke bestanden verwijdert. Hoewel ze in een tijdelijke map staan, kunnen ze zich opstapelen als ze verwaarloosd worden, waardoor ze een soort digitaal hamsternachtmerrie worden.

## Zie Ook
Voor verder lezen en een dieper begrip, moeten deze links zo ongeveer alles dekken wat je nodig hebt:

- De officiële documentatie van Microsoft over tijdelijke bestanden in .NET:
  [Tijdelijke bestanden in .NET](https://docs.microsoft.com/en-us/dotnet/standard/io/how-to-create-a-temporary-file)

- Beste praktijken voor het werken met bestanden en streams in C#:
  [Bestands- en Stream I/O](https://docs.microsoft.com/en-us/dotnet/standard/io)

- Als je de beveiligingsoverwegingen van bestand I/O wilt verkennen:
  [Bestand I/O en Beveiliging](https://docs.microsoft.com/en-us/dotnet/standard/security/secure-file-i-o)
