---
date: 2024-01-20 17:28:42.102902-07:00
description: 'Comment faire : Voici comment on joue avec les dates en C# .'
lastmod: '2024-03-13T22:44:57.802084-06:00'
model: gpt-4-1106-preview
summary: Voici comment on joue avec les dates en C#.
title: "Calcul d'une date future ou pass\xE9e"
weight: 26
---

## Comment faire :
Voici comment on joue avec les dates en C# :

```C#
using System;

class ManipulationDates
{
    static void Main()
    {
        DateTime today = DateTime.Now;
        
        // Ajouter 10 jours
        DateTime futureDate = today.AddDays(10);
        Console.WriteLine(futureDate.ToShortDateString());
        
        // Enlever 5 jours
        DateTime pastDate = today.AddDays(-5);
        Console.WriteLine(pastDate.ToShortDateString());
    }
}
```

Si aujourd'hui c'est le 12 avril 2023, le programme affichera :

```
22/04/2023
07/04/2023
```

## Exploration :
**Contexte historique :** Avant .NET, on manipulait les dates en C# avec plus de peine. `DateTime` a simplifié la vie. 

**Alternatives :** On peut aussi utiliser `TimeSpan` pour des durées, ou `DateTimeOffset` pour les fuseaux horaires. Il y a la bibliothèque NodaTime aussi, pour les casse-têtes de date et heure.

**Détails d'implémentation :** `AddDays` est pratique mais attention aux années bissextiles et changements d'heure. C# s'occupe de ces détails, mais il faut quand même tester pour des cas spéciaux.

## Voir Aussi :
- Documentation Microsoft sur DateTime : [docs.microsoft.com/dotnet/api/system.datetime](https://docs.microsoft.com/en-us/dotnet/api/system.datetime)
- NodaTime, pour aller plus loin : [nodatime.org](https://nodatime.org/)
