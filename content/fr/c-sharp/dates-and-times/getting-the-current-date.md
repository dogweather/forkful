---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:09:13.155468-07:00
description: "Obtenir la date actuelle en C# consiste \xE0 r\xE9cup\xE9rer les d\xE9\
  tails de la date et de l'heure actuelles du syst\xE8me. Les programmeurs ont souvent\
  \ besoin\u2026"
lastmod: '2024-03-11T00:14:31.748381-06:00'
model: gpt-4-0125-preview
summary: "Obtenir la date actuelle en C# consiste \xE0 r\xE9cup\xE9rer les d\xE9tails\
  \ de la date et de l'heure actuelles du syst\xE8me. Les programmeurs ont souvent\
  \ besoin\u2026"
title: Obtenir la date actuelle
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?
Obtenir la date actuelle en C# consiste à récupérer les détails de la date et de l'heure actuelles du système. Les programmeurs ont souvent besoin d'accéder à cette information pour la journalisation, le marquage temporel des opérations ou la planification des tâches au sein des applications, garantissant que les actions sont chronométrées avec précision et que les données sont marquées avec des horodatages précis.

## Comment faire :
C# offre une manière simple d'obtenir la date actuelle en utilisant la classe `DateTime` qui fait partie de l'espace de noms System du .NET Framework. L'exemple ci-dessous montre comment obtenir la date actuelle, et éventuellement, l'heure.

```csharp
using System;

class Program
{
    static void Main()
    {
        // Obtient uniquement la date actuelle
        DateTime currentDate = DateTime.Today;
        Console.WriteLine(currentDate.ToString("d"));  // Sortie : MM/jj/aaaa
        
        // Obtient la date et l'heure actuelles
        DateTime currentDateTime = DateTime.Now;
        Console.WriteLine(currentDateTime.ToString()); // Sortie : MM/jj/aaaa HH:mm:ss

        // Obtient la date et l'heure actuelles en UTC
        DateTime currentUtcDateTime = DateTime.UtcNow;
        Console.WriteLine(currentUtcDateTime.ToString()); // Sortie : MM/jj/aaaa HH:mm:ss
    }
}
```

En termes de bibliothèques tierces, NodaTime offre une alternative robuste pour la manipulation des dates et des heures, y compris la récupération de la date actuelle dans différents calendriers et fuseaux horaires.

```csharp
using NodaTime;
using System;

class Program
{
    static void Main()
    {
        // Utilisation de NodaTime pour obtenir la date actuelle dans le calendrier ISO
        LocalDate currentDate = SystemClock.Instance.GetCurrentInstant().InUtc().Date;
        Console.WriteLine(currentDate.ToString()); // Sortie : aaaa-MM-jj

        // Pour les dates spécifiques à un fuseau horaire
        DateTimeZone zone = DateTimeZoneProviders.Tzdb["America/New_York"];
        LocalDate currentZonedDate = SystemClock.Instance.GetCurrentInstant().InZone(zone).Date;
        Console.WriteLine(currentZonedDate.ToString()); // Sortie : aaaa-MM-jj
    }
}
```

Cela montre l'utilisation basique avec la classe `DateTime` intégrée et les capacités améliorées fournies par NodaTime, particulièrement utile pour les applications nécessitant la gestion de différents fuseaux horaires ou systèmes de calendrier.
