---
title:                "Analyse d'une date à partir d'une chaîne de caractères"
date:                  2024-01-20T15:35:17.438606-07:00
simple_title:         "Analyse d'une date à partir d'une chaîne de caractères"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/c-sharp/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## What & Why? (Quoi et Pourquoi ?)
Analyser une date depuis une chaîne permet de la transformer en un objet `DateTime` pour la manipuler aisément en C#. On le fait parce que les dates en texte sont courantes dans des fichiers ou des entrées utilisateur, et on a besoin de les traiter et de les comparer.

## How to: (Comment faire : )
Voici comment faire avec `DateTime.Parse` et `DateTime.TryParse`:
```C#
using System;
using System.Globalization;

class DateParsingExample
{
    static void Main()
    {
        // Utiliser DateTime.Parse
        string dateString = "24/01/2023";
        DateTime parsedDate = DateTime.Parse(dateString, new CultureInfo("fr-FR"));
        Console.WriteLine(parsedDate);  // Affichage: 24/01/2023 00:00:00

        // Utiliser DateTime.TryParse
        DateTime tryParsedDate;
        if (DateTime.TryParse(dateString, new CultureInfo("fr-FR"), DateTimeStyles.None, out tryParsedDate))
        {
            Console.WriteLine(tryParsedDate);  // Affichage: 24/01/2023 00:00:00
        }
        else
        {
            Console.WriteLine("Échec de l'analyse de la date.");
        }
    }
}
```

## Deep Dive (Plongée en profondeur)
Historiquement, analyser des dates était compliqué à cause des formats différents. On utilise `DateTime.Parse` quand on est sûr du format. Si la chaîne est invalide, ça lance une exception. C'est là qu'arrive `DateTime.TryParse`. C'est plus sûr, on obtient `false` au lieu d'une exception si ça rate. C'est utile lorsqu'on n'est pas certain de la validité de la chaîne de caractères.

Il y a aussi `ParseExact` et `TryParseExact` pour des formats spécifiques. Et puis, n'oubliez pas le débat sur la performance - `TryParse` est plus lent mais plus sûr.

## See Also (Voir Aussi)
- Documentation sur `DateTime.Parse`: [Microsoft Docs - DateTime.Parse](https://docs.microsoft.com/fr-fr/dotnet/api/system.datetime.parse)
- Documentation sur `DateTime.TryParse`: [Microsoft Docs - DateTime.TryParse](https://docs.microsoft.com/fr-fr/dotnet/api/system.datetime.tryparse)
- Culture et formats de dates: [Microsoft Docs - Culture and Date Formats](https://docs.microsoft.com/fr-fr/dotnet/standard/base-types/standard-date-and-time-format-strings)
- Parse vs TryParse: [Stack Overflow discussion](https://stackoverflow.com/questions/919244/converting-a-string-to-datetime)
