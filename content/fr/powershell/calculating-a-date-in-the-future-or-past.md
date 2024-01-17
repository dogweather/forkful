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

## Quoi & Pourquoi?
Calculer une date dans le futur ou le passé est une fonctionnalité utile et couramment utilisée par les programmeurs dans différents domaines tels que les finances, l'informatique ou les logiciels. Cette tâche consiste à déterminer une date en utilisant une date de référence et un intervalle de temps spécifié, que ce soit en ajoutant ou en soustrayant des jours, des mois, des années, des heures ou des minutes.

La raison pour laquelle les programmeurs utilisent cette fonctionnalité est principalement liée à la nécessité de gérer des événements planifiés ou de calculer des échéances. En utilisant une date de référence et un intervalle, il est possible de déterminer facilement une date future ou une date passée, ce qui peut être très pratique pour automatiser des tâches ou générer des rappels.

## Comment faire:

```PowerShell
# Calculer une date dans le futur en ajoutant 10 jours à la date actuelle
(Get-Date).AddDays(10)

# Sortie:     lundi 16 décembre 2019 21:41:07

# Calculer une date dans le passé en soustrayant 2 mois à la date actuelle
(Get-Date).AddMonths(-2)

# Sortie:     dimanche 13 octobre 2019 21:41:07

# Calculer une date en utilisant une date de référence spécifique et un intervalle de temps
(Get-Date "2020-01-01").AddHours(5)

# Sortie:     mercredi 1 janvier 2020 05:00:00
```

La première ligne de code utilise la méthode ```AddDays()``` pour ajouter 10 jours à la date actuelle, tandis que la deuxième ligne utilise la méthode ```AddMonths()``` pour soustraire 2 mois à la date actuelle. La troisième ligne illustre l'utilisation d'une date de référence spécifique, suivie de la méthode ```AddHours()``` pour ajouter 5 heures à cette date.

## Plongée en profondeur:
La fonction de calcul de date existe depuis les premiers langages de programmation et est souvent incluse dans les bibliothèques de fonctions standard. Dans PowerShell, on peut utiliser la méthode ```AddX()``` avec un intervalle positif ou ```SubtractX()``` avec un intervalle négatif pour atteindre le même résultat.

En plus de la méthode ```AddX()```, PowerShell propose également la méthode ```Add()``` qui permet de spécifier l'unité de temps en utilisant l'énumération ```TimeSpan```. Par exemple, on peut écrire ```Add([TimeSpan]::FromDays(5))``` pour ajouter 5 jours à une date. 

Il existe également d'autres méthodes utiles pour manipuler les dates, telles que ```Get-Date``` pour obtenir la date et l'heure actuelles, ```Parse()``` pour convertir une chaîne en objet date, et ```ToString()``` pour formater la date selon un modèle souhaité.

## Voir aussi:
Documentation officielle de Microsoft sur la fonction ```AddDays()```: https://docs.microsoft.com/fr-fr/dotnet/api/system.datetime.adddays

Exemple d'utilisation de la fonction dans un script PowerShell: https://www.powershellgallery.com/packages/GetDateFromToday/1.0/Content/Public\Test-GetDateFromToday.ps1