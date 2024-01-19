---
title:                "Calculer une date dans le futur ou le passé"
html_title:           "PowerShell: Calculer une date dans le futur ou le passé"
simple_title:         "Calculer une date dans le futur ou le passé"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/powershell/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Qu'est-ce que c'est & Pourquoi ?

Calculer une date à l'avenir ou au passé, c'est simplement trouver une date relative à une autre date, typiquement la date actuelle. Les programmeurs font ça pour une variété de raisons - pour fixer une date d’expiration, planifier une tâche à exécuter plus tard, etc.

## Comment faire :

Voici comment ajouter ou soustraire des jours à la date actuelle en PowerShell. 

```PowerShell
# Pour ajouter des jours
$date = Get-Date
$futureDate = $date.AddDays(7)
Write-Output $futureDate

# Pour soustraire des jours
$pastDate = $date.AddDays(-7)
Write-Output $pastDate
```

À exécution, on obtient :

```Shell
# Exemple de sortie
12 avril 2022 16:50:14
5 avril 2022 16:50:14
```

## Deep Dive

Historiquement, les calculs de dates étaient un peu compliqués à cause des différentes normes de calendrier. PowerShell, cependant, utilise le calendrier Grégorien ISO 8601, qui est largement adopté à travers le monde.

Comme alternatives, vous pouvez utiliser `AddMonths` pour ajouter ou soustraire des mois, ou `AddYears` pour des années. Faites juste attention, PowerShell respecte les durées des mois varies et les années bissextiles !

Détail d'implémentation : `AddDays`, `AddMonths` et `AddYears` sont des méthodes de l'objet `DateTime` dans .NET, que PowerShell utilise sous le capot.

## Voir aussi :

Pour plus d'informations sur le calcul de la date en PowerShell :

1. Documentation Microsoft sur `Get-Date` : https://docs.microsoft.com/fr-fr/powershell/module/microsoft.powershell.utility/get-date?view=powershell-7.1
2. Discussion StackOverflow sur le calcul de la date : https://stackoverflow.com/questions/9533114/get-date-getting-previous-day-date-in-powershell
3. Tutoriel sur le calcul de la date : https://www.computerperformance.co.uk/powershell/date-math/