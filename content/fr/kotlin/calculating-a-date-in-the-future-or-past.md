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

## Pourquoi

Il peut être utile de calculer une date dans le futur ou dans le passé lors de la programmation en Kotlin, par exemple pour planifier des tâches ou des événements. Heureusement, Kotlin offre des fonctionnalités simples et efficaces pour effectuer ces calculs.

## Comment faire

La première étape pour calculer une date dans le futur ou dans le passé consiste à utiliser la classe `LocalDate` de Kotlin, qui représente une date sans prendre en compte les heures et les minutes. Nous pouvons créer cette classe en utilisant la méthode `now()` qui prend en paramètre le fuseau horaire souhaité. Par exemple :

```
Kotlin
val today = LocalDate.now(ZoneId.systemDefault())
```

Une fois que nous avons notre date de référence, nous pouvons ajouter ou soustraire des jours, des mois ou des années en utilisant les méthodes `plus()` ou `minus()` en spécifiant le nombre d'unités souhaitées. Voici un exemple qui calcule la date dans une semaine à partir de la date de référence :

```
Kotlin
val futureDate = today.plus(1, ChronoUnit.WEEKS)
```

Nous pouvons également spécifier une date précise en utilisant la méthode `of()` qui prend en paramètres l'année, le mois et le jour souhaités. Par exemple :

```
Kotlin
val date = LocalDate.of(2022, 5, 9)
```

Pour plus de flexibilité, Kotlin nous permet également d'utiliser la classe `LocalDateTime` qui prend en compte les heures et les minutes. Ainsi, nous pouvons spécifier une heure précise en utilisant la méthode `atTime()`. Par exemple :

```
Kotlin
val dateTime = date.atTime(9, 30)
```

Nous pouvons également utiliser d'autres classes telles que `ZonedDateTime` ou `OffsetDateTime` pour gérer les fuseaux horaires et les décalages horaires.

## Plongée en profondeur

En utilisant les classes telles que `LocalDate` ou `LocalDateTime`, Kotlin nous permet de manipuler facilement les dates dans le futur ou dans le passé en éliminant la complexité liée aux fuseaux horaires et aux décalages horaires. De plus, en utilisant des méthodes telles que `plus()` ou `minus()`, nous avons la possibilité de spécifier des unités de temps variées, ce qui rend le calcul de dates encore plus flexible.

## Voir aussi

- Documentation officielle de Kotlin sur `LocalDate` : https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.time/-local-date/
- Tutoriel sur les dates et heures en Kotlin : https://www.tutorialspoint.com/kotlin/kotlin_date_time.htm
- Documentation sur les classes de temps en Kotlin : https://www.programiz.com/kotlin-programming/datetime