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

# Comment calculer une date future ou passée en Kotlin?

## Qu'est-ce et pourquoi?

Calculer une date future ou passée consiste à manipuler des dates pour trouver un jour avant ou après une date donnée. Les programmeurs effectuent cette opération pour accomplir une multitude de tâches, telles que la planification de notifications ou le suivi de périodes précises.

## Comment faire:

Kotlin fournit la bibliothèque java.time pour manipuler les dates. Voici comment vous pouvez calculer une date future ou passée.

Pour ajouter à une date:

```kotlin
import java.time.LocalDate

fun main() {
    val currentDate = LocalDate.now()
    val futureDate = currentDate.plusDays(5)
    println("Future Date: $futureDate")
}
```

Pour soustraire d'une date:

```kotlin
import java.time.LocalDate

fun main() {
    val currentDate = LocalDate.now()
    val pastDate = currentDate.minusDays(5)
    println("Past Date: $pastDate")
}
```

La sortie sera une date qui est 5 jours avant ou 5 jours après la date actuelle.

## Exploration détaillée:

Historiquement, le calcul des dates était complexe et rempli de pièges grâce aux variations de durée des mois et années. Avec l'introduction de la bibliothèque java.time dans Java 8 et Kotlin, ces problèmes ont été largement surmontés.

Autres alternatives: avant java.time, les programmeurs utilisaient des bibliothèques comme Joda-Time pour manipuler les dates. Malgré la mise à la retraite de Joda-Time pour les nouvelles applications, elle reste une référence historique incontournable.

Détails d'implémentation: la manipulation des dates avec java.time est largement basée sur l'horloge système par défaut. `LocalDate.now()` utilise l'horloge système par défaut pour obtenir la date actuelle. `plusDays()` et `minusDays()` ajoutent ou soustraient des jours basés sur la durée standard de 24 heures du jour.

## Voir aussi:

2. [Guide pratique pour Joda-Time](http://www.joda.org/joda-time/quickstart.html)
3. [Guide Java sur le travail avec java.time](https://www.baeldung.com/java-8-date-time-intro)