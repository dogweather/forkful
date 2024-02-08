---
title:                "Conversion d'une date en chaîne de caractères"
aliases:
- fr/powershell/converting-a-date-into-a-string.md
date:                  2024-01-20T17:37:14.985642-07:00
model:                 gpt-4-1106-preview
simple_title:         "Conversion d'une date en chaîne de caractères"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/powershell/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## What & Why?
Convertir une date en chaîne de caractères permet de la formater pour l'affichage ou le stockage. Les programmeurs font ça pour la lisibilité et pour respecter les normes de formatage locales.

## How to:
PowerShell rend la tâche aisée avec la cmdlet `Get-Date` et la méthode `.ToString()`. Voici quelques tours de magie :

```PowerShell
# Affichage de la date actuelle sous forme de chaîne
$dateActuelle = Get-Date
$dateEnChaine = $dateActuelle.ToString()
$dateEnChaine

# Formatage spécifique : "jour/mois/année heures:minutes"
$dateFormattee = $dateActuelle.ToString("dd/MM/yyyy HH:mm")
$dateFormattee

# Formatage ISO 8601
$dateISO = $dateActuelle.ToString("o")
$dateISO
```

Sortie typique :

```
dimanche 9 avril 2023 14:29:35
09/04/2023 14:29
2023-04-09T14:29:35.0000000+02:00
```

## Deep Dive
Historiquement, les dates en chaîne étaient un casse-tête en raison des formats divers. Avec PowerShell, on utilise des formats standardisés (comme ISO 8601) ou des formats personnalisés via des chaînes de format.

Alternatives : outre la méthode `.ToString()`, on peut utiliser la méthode `.ToShortDateString()` pour un format court ou `.ToLongDateString()` pour un format complet.

Détails d'implémentation : `.ToString()` utilise la culture courante de votre système, qui détermine le format de date. Pour spécifier une culture, utilisez la surcharge `.ToString($format, [System.Globalization.CultureInfo]::InvariantCulture)`.

## See Also
- [Custom date and time format strings](https://docs.microsoft.com/en-us/dotnet/standard/base-types/custom-date-and-time-format-strings)
- [CultureInfo Class](https://docs.microsoft.com/en-us/dotnet/api/system.globalization.cultureinfo?view=net-6.0)
