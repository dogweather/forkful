---
title:                "Obtenir la date actuelle"
date:                  2024-01-20T15:15:43.106330-07:00
html_title:           "C: Obtenir la date actuelle"
simple_title:         "Obtenir la date actuelle"

category:             "PowerShell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/powershell/getting-the-current-date.md"
---

{{< edit_this_page >}}

## What & Why?
Obtenir la date actuelle dans un script, c'est facile et über utile. Programmation d'échéances, logs, ou pour juste savoir le jour, on fait ça tout le temps.

## How to:
```PowerShell
# Obtenir la date et l'heure actuelles
Get-Date

# Format personnalisé de la date
Get-Date -Format "yyyy-MM-dd"

# Seulement l'heure
Get-Date -Format "HH:mm:ss"
```

Sortie exemple pour le deuxième exemple:
```
2023-04-05
```

## Deep Dive
En PowerShell, `Get-Date` est votre baguette magique pour tout ce qui est temps et date. C'est un héritage de la nuit des temps informatiques, où déjà on timbrait des événements et analyses.

Alternative? Oui, `[System.DateTime]::Now`, mais pourquoi se compliquer la vie ?

Les détails ? `Get-Date` vous offre le pouvoir de formater la sortie avec presque tous les morceaux d'une date dont vous pourriez rêver. Des millisecondes aux fuseaux horaires, tentez, experimentez.

## See Also
- [Get-Date Documentation](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.utility/get-date?view=powershell-7.1)
- [Standard Date and Time Format Strings](https://docs.microsoft.com/en-us/dotnet/standard/base-types/standard-date-and-time-format-strings)
- [Custom Date and Time Format Strings](https://docs.microsoft.com/en-us/dotnet/standard/base-types/custom-date-and-time-format-strings)
