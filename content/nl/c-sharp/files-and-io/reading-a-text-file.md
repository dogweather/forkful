---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:04:58.410052-07:00
description: "Een tekstbestand lezen is het ophalen van gegevens uit een bestand dat\
  \ tekst bevat. Programmeurs doen dit om configuraties te laden, gegevens te lezen\
  \ of\u2026"
lastmod: '2024-03-11T00:14:24.655145-06:00'
model: gpt-4-0125-preview
summary: "Een tekstbestand lezen is het ophalen van gegevens uit een bestand dat tekst\
  \ bevat. Programmeurs doen dit om configuraties te laden, gegevens te lezen of\u2026"
title: Een tekstbestand lezen
---

{{< edit_this_page >}}

## Wat & Waarom?
Een tekstbestand lezen is het ophalen van gegevens uit een bestand dat tekst bevat. Programmeurs doen dit om configuraties te laden, gegevens te lezen of bronnen op te halen die te omvangrijk of ongeschikt zijn om hard in de code te zetten.

## Hoe te:
Laten we er direct induiken. Hier is hoe je uit een bestand leest in C# met `System.IO`.

```C#
using System;
using System.IO;

class Program
{
    static void Main()
    {
        string filePath = @"C:\pad\naar\jouw\bestand.txt";
        
        // Alle tekst lezen
        string allText = File.ReadAllText(filePath);
        Console.WriteLine(allText);
        
        // Regels in een array lezen
        string[] lines = File.ReadAllLines(filePath);
        foreach (var line in lines)
        {
            Console.WriteLine(line);
        }
        
        // Lezen met een StreamReader
        using (StreamReader reader = new StreamReader(filePath))
        {
            string line;
            while ((line = reader.ReadLine()) != null)
            {
                Console.WriteLine(line);
            }
        }
    }
}
```

Voorbeelduitvoer:

```
Hallo, dit is een tekstbestand.
Het heeft meerdere regels.
Elke regel zal afzonderlijk worden gelezen.
```

## Diepgaande duik
Een tekstbestand lezen lijkt eenvoudig genoeg, toch? Maar er is een beetje geschiedenis en enkele nuances die het waard zijn om te weten.

Vroeger waren tekstbestanden vaak de primaire manier om gegevens op te slaan voordat databases algemeen gebruikt werden. Programmeurs moesten toegang tot bestanden beheren, gegevens correct formatteren en fouten afhandelen. C# is sindsdien enorm geëvolueerd. Nu is `System.IO` je standaard namespace voor bestandsbewerkingen.

Je hebt opties:

- `File.ReadAllText` leest alles in één keer - geweldig voor kleinere bestanden.
- `File.ReadAllLines` geeft je elke regel als een array-element - handig voor het verwerken van regels.
- `StreamReader` leest regel voor regel, wat efficiënter is qua geheugen voor grote bestanden.

Elke methode vergrendelt het bestand terwijl het in gebruik is. Dit is belangrijk als andere processen mogelijk proberen toegang te krijgen tot het bestand.

Onthoud, behandel altijd uitzonderingen zoals `FileNotFoundException` of `IOException` bij het omgaan met bestanden. Je wilt niet dat je app onverwacht crasht.

## Zie ook
Heb je meer vragen of wil je je kennis uitbreiden? Bekijk deze links:

- [MSDN Documentatie over File Class](https://docs.microsoft.com/nl-nl/dotnet/api/system.io.file?view=netcore-3.1)
- [MSDN Documentatie over StreamReader Class](https://docs.microsoft.com/nl-nl/dotnet/api/system.io.streamreader?view=netcore-3.1)
- [Tutorial over uitzonderingsafhandeling](https://docs.microsoft.com/nl-nl/dotnet/csharp/programming-guide/exceptions/)
