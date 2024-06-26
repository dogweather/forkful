---
date: 2024-01-20 17:36:01.778499-07:00
description: "How to: La conversion de dates en cha\xEEnes de caract\xE8res est un\
  \ concept ancien en programmation. En C#, `DateTime.ToString()` a toujours \xE9\
  t\xE9 le moyen\u2026"
lastmod: '2024-04-05T21:53:59.285440-06:00'
model: gpt-4-1106-preview
summary: "La conversion de dates en cha\xEEnes de caract\xE8res est un concept ancien\
  \ en programmation."
title: "Conversion d'une date en cha\xEEne de caract\xE8res"
weight: 28
---

## How to:
```C#
using System;
using System.Globalization;

public class DateToStringExample
{
    public static void Main()
    {
        DateTime now = DateTime.Now;
        // Formatage simple
        Console.WriteLine(now.ToString("dd-MM-yyyy")); // Exemple de sortie : 06-04-2023
        
        // Formatage avec nom du jour
        Console.WriteLine(now.ToString("dddd, dd MMMM yyyy")); // Exemple de sortie : jeudi, 06 avril 2023
        
        // Formatage ISO 8601
        Console.WriteLine(now.ToString("yyyy-MM-ddTHH:mm:ssK")); // Exemple de sortie : 2023-04-06T14:03:57+02:00
    }
}
```

## Deep Dive
La conversion de dates en chaînes de caractères est un concept ancien en programmation. En C#, `DateTime.ToString()` a toujours été le moyen privilégié. Cependant, la méthode a évolué pour supporter plus de formats comme `DateTimeOffset` pour les fuseaux horaires. La méthode `ToString()` peut utiliser des formats standard ou personnalisés. Il existe d'autres bibliothèques telles que NodaTime offrant plus de contrôle sur les dates et heures.

Côté implémentation, la classe `DateTime` utilise `DateTimeFormatInfo` issu de `System.Globalization`. Cela permet d'adapter le formatage aux conventions locales. Par exemple, `CultureInfo` permet de formater la date pour un public français :

```C#
Console.WriteLine(now.ToString("f", CultureInfo.CreateSpecificCulture("fr-FR"))); // Exemple de sortie : jeudi 6 avril 2023 14:03
```

## See Also
- [Documentation officielle de DateTime.ToString](https://docs.microsoft.com/en-us/dotnet/api/system.datetime.tostring)
- [Guide pour les formats de date et heure standard en C#](https://docs.microsoft.com/en-us/dotnet/standard/base-types/standard-date-and-time-format-strings)
- [Guide pour les formats de date et heure personnalisés en C#](https://docs.microsoft.com/en-us/dotnet/standard/base-types/custom-date-and-time-format-strings)
- [NodaTime, une bibliothèque alternative de manipulation de dates](https://nodatime.org/)
