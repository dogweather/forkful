---
title:    "Kotlin: Calculer une date dans le futur ou le passé"
keywords: ["Kotlin"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/kotlin/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Pourquoi

La capacité à calculer une date dans le futur ou dans le passé peut être utile pour de nombreuses raisons, telles que planifier un événement ou calculer des délais de livraison. En utilisant Kotlin, nous pouvons facilement réaliser ces calculs pour nous, plutôt que de les faire manuellement.

## Comment faire

Voici un exemple de code en Kotlin pour calculer une date dans le futur ou dans le passé :

```
// Calculer la date dans le futur en utilisant l'opérateur "plus" du type "LocalDate"
val jourFutur = LocalDate.now().plus(2, ChronoUnit.DAYS)
println(jourFutur)

// Calculer la date dans le passé en utilisant l'opérateur "minus" du type "LocalDate"
val jourPasse = LocalDate.now().minus(2, ChronoUnit.MONTHS)
println(jourPasse)
```

Output :
```
2020-09-23 (pour "jourFutur")
2020-05-23 (pour "jourPasse")
```

Il est également possible de calculer une date à partir d'une date existante :

```
// Utilisation d'une date existante
val date = LocalDate.of(2020, Month.JULY, 15)

// Calculer la date 1 mois plus tard et l'imprimer
val futur = date.plusMonths(1)
println(futur)

// Calculer la date 2 semaines plus tôt et l'imprimer
val passe = date.minusWeeks(2)
println(passe)
```

Output :
```
2020-08-15 (pour "futur")
2020-07-01 (pour "passe")
```

## Plongée en profondeur

En utilisant la classe "LocalDate" de Kotlin, nous pouvons facilement manipuler des dates en ajoutant ou en soustrayant des unités de temps telles que des jours, des mois ou des années. Il est également possible de calculer des dates en utilisant des unités de temps personnalisées telles que des semaines ou des heures en utilisant la classe "ChronoUnit".

De plus, Kotlin offre également la possibilité de formater les dates selon différents modèles tels que "dd-MM-yyyy" ou "MM/dd/yyyy" en utilisant la méthode "format".

## Voir aussi

- Documentation officielle de Kotlin pour les classes "LocalDate" et "ChronoUnit" : [Accessing and Modifying Dates](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.time/java.time.-date-time/index.html)

- Tutoriel sur la manipulation des dates en Kotlin : [Working with Dates in Kotlin](https://www.raywenderlich.com/2732191-working-with-dates-in-kotlin)