---
title:                "Calculer une date dans le futur ou le passé"
html_title:           "Kotlin: Calculer une date dans le futur ou le passé"
simple_title:         "Calculer une date dans le futur ou le passé"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/kotlin/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

# Calculer des dates dans le passé ou dans le futur avec Kotlin

## Quoi & Pourquoi?
Calculer une date dans le passé ou dans le futur consiste simplement à trouver une date qui se trouve avant ou après une date donnée. Les programmeurs le font souvent pour des tâches telles que la planification d'événements ou le suivi du temps écoulé entre deux dates.

## Comment faire:
Voici quelques exemples de code en Kotlin pour calculer des dates dans le passé ou dans le futur:

```Kotlin
// Calculer une date dans le futur
val dateActuelle = LocalDate.now()
val dateDansUnAn = dateActuelle.plusYears(1)
println("La date dans un an sera $dateDansUnAn")

// Calculer une date dans le passé
val dateActuelle = LocalDate.now()
val dateIlYaUneSemaine = dateActuelle.minusWeeks(1)
println("Il y a une semaine, c'était $dateIlYaUneSemaine")
```
Résultat:

```
La date dans un an sera 2021-04-16
Il y a une semaine, c'était 2021-04-09
```

## Plongée en profondeur:
Historiquement, les calculs de dates ont été un défi pour les programmeurs en raison de la complexité du temps et des différentes façons dont les dates sont calculées dans les différents calendriers. Heureusement, les langages de programmation modernes comme Kotlin offrent des bibliothèques intégrées pour gérer facilement ces calculs.

Il existe également des alternatives à la manipulation de dates en utilisant la classe d'origine de Kotlin, telle que l'utilisation d'une bibliothèque externe comme Joda-Time ou en utilisant des outils de gestion de dates dans les bases de données.

Pour implémenter le calcul de dates dans le passé ou dans le futur, Kotlin utilise le concept de "fluent API" qui permet d'enchainer plusieurs méthodes sur un même objet.

## Voir aussi:
- [Documentation officielle de Kotlin sur la manipulation de dates](https://kotlinlang.org/docs/datetime.html)
- [Article Medium sur le calcul de dates avec Kotlin](https://medium.com/@mohitsharma_80641/date-and-calendar-apis-in-kotlin-d31ee1614032)
- [Documentation officielle de Joda-Time](https://www.joda.org/joda-time/)