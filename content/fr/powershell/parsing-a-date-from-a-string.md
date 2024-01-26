---
title:                "Analyse d'une date à partir d'une chaîne de caractères"
date:                  2024-01-20T15:37:56.293700-07:00
html_title:           "Arduino: Analyse d'une date à partir d'une chaîne de caractères"
simple_title:         "Analyse d'une date à partir d'une chaîne de caractères"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/powershell/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## What & Why?
La conversion d'une date depuis une chaîne de caractères permet de traiter et manipuler les informations temporelles. Les programmeurs font cela pour comparer, trier ou stocker des dates de manière cohérente.

## How to:
Pour analyser une date à partir d'une chaîne de caractères en PowerShell, utilisez la cmdlet `Get-Date` avec le paramètre `-Format` pour spécifier le format de sortie.

```PowerShell
# Analyse une date en format standard
$dateString = "2023-03-14"
$date = Get-Date $dateString
echo $date

# Spécifie un format de date personnalisé
$dateStringCustom = "14 mars 2023 15:00"
$dateCustom = Get-Date $dateStringCustom -Format "dd MMMM yyyy HH:mm"
echo $dateCustom
```

Résultats :
```
mardi 14 mars 2023 00:00:00
14 mars 2023 15:00
```

## Deep Dive
Historiquement, les programmeurs devaient écrire des fonctions de conversion complexes pour les dates. En PowerShell, `Get-Date` simplifie la tâche, en gérant de nombreux formats de date et heure.

Il existe des alternatives, comme utiliser la méthode `Parse` ou `ParseExact` de l'objet `[datetime]` pour des analyses plus précises ou complexes :

```PowerShell
# Utilisation de ParseExact
$format = "dd-MM-yyyy HH:mm:ss"
$dateStringExact = "14-03-2023 15:00:00"
$dateExact = [datetime]::ParseExact($dateStringExact, $format, $null)
echo $dateExact
```

L'implémentation est fondée sur la culture (.NET CultureInfo) de l'utilisateur. Par défaut, PowerShell utilise la culture définie sur le système d'exploitation, mais vous pouvez la modifier si nécessaire avec `[CultureInfo]`.

## See Also
Pour explorer plus loin, consultez :
- [Get-Date documentation](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.utility/get-date?view=powershell-7.1)
- [CultureInfo Class](https://docs.microsoft.com/en-us/dotnet/api/system.globalization.cultureinfo?view=net-6.0)
- [Format des dates et heures standard](https://docs.microsoft.com/fr-fr/dotnet/standard/base-types/standard-date-and-time-format-strings)
- [Format des dates et heures personnalisés](https://docs.microsoft.com/fr-fr/dotnet/standard/base-types/custom-date-and-time-format-strings)
