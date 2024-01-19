---
title:                "Comparer deux dates"
html_title:           "Clojure: Comparer deux dates"
simple_title:         "Comparer deux dates"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/powershell/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi?

Comparer deux dates consiste à déterminer si une date est antérieure, égale ou postérieure à une autre date. Les programmeurs le font couramment pour effectuer des opérations basées sur le temps, comme la planification des tâches ou le tri des données par date.

## Comment faire:

Dans PowerShell, vous pouvez comparer les dates en utilisant les opérateurs de comparaison `-lt`, `-eq`, `-gt`. Voici un exemple:

```PowerShell
$date1 = Get-Date
$difference = New-TimeSpan -Start $date1 -End $date2

if ($difference -gt 0) {
    Write-Output "La date2 est postérieure à la date1"
} else {
    Write-Output "La date1 est égale ou postérieure à la date2"
}
```

Dans cet exemple, si `date2` est postérieur à `date1`, le programme affichera: "La date2 est postérieure à la date1". Sinon, il affichera: "La date1 est égale ou postérieure à la date2".

## Exploration approfondie

Historiquement, comparer des dates en programmation était complexe et sujette à des erreurs, en raison des différentes manières de représenter et de stocker les dates. Heureusement, PowerShell, avec son objet `DateTime`, simplifie grandement cette tâche.

Il existe des alternatives à l'utilisation du module `DateTime` intégré de PowerShell. Vous pouvez utiliser des fonctions personnalisées ou des modules externes. Cependant, `DateTime` reste le moyen le plus direct et efficace de comparer deux dates.

Le détail d'implémentation important à noter est que `DateTime` gère toujours les dates en tenant compte du fuseau horaire local. Si vous avez besoin de travailler avec des dates et des heures universelles, vous devriez utiliser `[DateTime]::UtcNow`.

## Voir aussi:

[Documentation officielle de Microsoft sur l'utilisation de DateTime](https://docs.microsoft.com/fr-fr/dotnet/api/system.datetime?view=net-5.0)

[Tutoriel sur l'utilisation des dates et des heures en PowerShell](https://learn-powershell.net/2012/10/14/powershell-and-working-with-dates-and-time/) 

[Article sur la comparaison des dates en PowerShell](https://www.red-gate.com/simple-talk/sysadmin/powershell/powershell-day-to-day-admin-tasks-manipulations-with-datetime/)