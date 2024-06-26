---
changelog:
- 2024-01-30, gpt-4-0125-preview, translated from English
date: 2024-01-30 19:10:09.070086-07:00
description: "Hoe te: In C# werk je met associatieve arrays door gebruik te maken\
  \ van de `Dictionary<TKey, TValue>` klasse. Hier is een snel voorbeeld om je op\
  \ weg te\u2026"
lastmod: '2024-03-13T22:44:50.803014-06:00'
model: gpt-4-0125-preview
summary: In C# werk je met associatieve arrays door gebruik te maken van de `Dictionary<TKey,
  TValue>` klasse.
title: Gebruik van associatieve arrays
weight: 15
---

## Hoe te:
In C# werk je met associatieve arrays door gebruik te maken van de `Dictionary<TKey, TValue>` klasse. Hier is een snel voorbeeld om je op weg te helpen:

```C#
using System;
using System.Collections.Generic;

class Program
{
    static void Main()
    {
        // Een woordenboek creëren
        Dictionary<string, int> fruitmand = new Dictionary<string, int>();

        // Sleutel-waarde paren toevoegen
        fruitmand.Add("Appels", 5);
        fruitmand.Add("Sinaasappels", 10);

        // Een waarde opvragen met behulp van de sleutel
        Console.WriteLine("Appels: " + fruitmand["Appels"]);
        
        // Een waarde bijwerken
        fruitmand["Appels"] = 7;
        Console.WriteLine("Bijgewerkte Appels: " + fruitmand["Appels"]);
        
        // Een sleutel-waarde paar verwijderen
        fruitmand.Remove("Sinaasappels");

        // Door het woordenboek itereren
        foreach (var paar in fruitmand)
        {
            Console.WriteLine(paar.Key + ": " + paar.Value);
        }
    }
}
```
Voorbeelduitvoer:
```
Appels: 5
Bijgewerkte Appels: 7
Appels: 7
```

Dit voorbeeld toont het creëren van een woordenboek, toevoegen, toegang krijgen, bijwerken, en elementen verwijderen, en eroverheen itereren.

## Diepere Duik
Het concept van associatieve arrays gaat terug op hun gebruik in scripttalen zoals Perl en PHP, waar ze flexibiliteit bieden in het beheer van gegevenscollecties. In C# is `Dictionary<TKey, TValue>` de facto implementatie, geïntroduceerd in .NET Framework 2.0. Het slaat gegevens op in een hashtable, wat zorgt voor efficiënte zoekopdrachten, toevoegingen, en verwijderingen.

Het is echter de moeite waard om op te merken dat, hoewel woordenboeken ongelooflijk veelzijdig zijn, ze niet altijd je beste optie kunnen zijn. Voor het onderhouden van geordende collecties, kun je kijken naar `SortedDictionary<TKey, TValue>` of `SortedList<TKey, TValue>`, die gesorteerde volgorde bieden ten koste van langzamere invoeg- en verwijderingsoperaties. Voor scenario's die draadveiligheid vereisen, voegt `ConcurrentDictionary<TKey, TValue>` overhead toe maar verzekert veilige toegang vanuit meerdere threads zonder handmatige vergrendeling.

Uiteindelijk hangt de keuze voor een implementatie van associatieve arrays in C# af van je specifieke behoeften met betrekking tot volgorde, prestaties, en draadveiligheid.
