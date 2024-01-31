---
title:                "Controleren of een directory bestaat"
date:                  2024-01-28T21:56:00.050871-07:00
model:                 gpt-4-0125-preview
simple_title:         "Controleren of een directory bestaat"

category:             "C#"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/c-sharp/checking-if-a-directory-exists.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?
Het controleren van het bestaan van een map in C# stelt je in staat om te bevestigen of een specifieke map beschikbaar is in het bestandssysteem. Programmeurs doen dit om fouten te voorkomen, zoals proberen te lezen van of schrijven naar een niet-bestaande map, wat hun programma's zou doen crashen of onvoorspelbaar zou laten gedragen.

## Hoe:
Hier is hoe je kunt controleren of een map bestaat met behulp van de `System.IO` namespace:

```C#
using System;
using System.IO;

class Program
{
    static void Main()
    {
        string directoryPath = @"C:\exampleFolder";

        if (Directory.Exists(directoryPath))
        {
            Console.WriteLine("De map bestaat.");
        }
        else
        {
            Console.WriteLine("De map bestaat niet.");
        }
    }
}
```
Voorbeelduitvoer:
```
De map bestaat.
```
Of, als de map niet gevonden wordt:
```
De map bestaat niet.
```

## Diepere Duik
De `System.IO` namespace bestaat al sinds de vroege dagen van .NET, en biedt hulpmiddelen voor bestands- en mapbewerkingen. Bij het controleren van het bestaan van een map, maakt het onder de motorkap gebruik van de systeem-API om het bestandssysteem te bevragen -- een operatie die doorgaans goedkoop is qua systeembronnen.

Er is ook de `DirectoryInfo` klasse, die een objectgeoriënteerde manier biedt om met mappen te interageren. Dit kan langzamer zijn voor het alleen controleren van het bestaan, aangezien het een object creëert met meer gegevens dan alleen de bestaansstaat, maar het is handig voor meer complexe bewerkingen.

```C#
DirectoryInfo dirInfo = new DirectoryInfo(directoryPath);
if (dirInfo.Exists)
{
    // Doe iets met de map.
}
```

Voor `System.IO`, zouden ontwikkelaars platformspecifieke API's gebruikt kunnen hebben of command-line hulpmiddelen om te controleren of een map bestaat, beide zijn rommelig en vol met problemen. `System.IO` heeft dit mooi geabstraheerd.

Het is cruciaal om op te merken dat bestaanscontroles onderhevig kunnen zijn aan racecondities. Enkel omdat een map bestaat op het moment dat je controleert, garandeert dit niet dat het een moment later nog bestaat wanneer je het probeert te gebruiken, vanwege potentiële veranderingen door andere processen of gebruikers.

## Zie Ook
- [MSDN Documentatie over System.IO.Directory.Exists](https://docs.microsoft.com/en-us/dotnet/api/system.io.directory.exists)
- [MSDN Documentatie over System.IO.DirectoryInfo](https://docs.microsoft.com/en-us/dotnet/api/system.io.directoryinfo)
- [StackOverflow discussie over het controleren van mapbestaan](https://stackoverflow.com/questions/1410127/c-sharp-test-if-user-has-write-access-to-a-folder)
