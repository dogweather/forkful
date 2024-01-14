---
title:    "Kotlin: Calculer une date dans le futur ou le passé"
keywords: ["Kotlin"]
---

{{< edit_this_page >}}

# Pourquoi

Calculer une date dans le futur ou dans le passé peut être utile dans de nombreux cas, que ce soit pour programmer des rappels, planifier des événements ou simplement pour connaître une date précise à l'avance. Cela permet également d'automatiser certaines tâches et de gagner du temps.

## Comment faire

Pour calculer une date dans le futur ou dans le passé en Kotlin, vous pouvez utiliser la classe ```LocalDate``` de la bibliothèque standard Java.time. Voici un exemple de code qui affiche la date dans 30 jours à partir d'aujourd'hui:

```Kotlin
val today = LocalDate.now()
val futureDate = today.plusDays(30)
println(futureDate)
```

Cela produira la sortie suivante: `2021-10-14` (si aujourd'hui est le 14 septembre 2021).

Pour calculer une date dans le passé, vous pouvez utiliser la méthode `minus` au lieu de `plus`, en indiquant le nombre de jours à soustraire. Par exemple:

```Kotlin
val today = LocalDate.now()
val pastDate = today.minusDays(7)
println(pastDate)
```

Cela affichera `2021-09-07` (si aujourd'hui est le 14 septembre 2021).

## Plongée profonde

Il est important de noter que les méthodes `plusDays` et `minusDays` renvoient une nouvelle instance de la classe `LocalDate` plutôt que de modifier l'objet existant. Cela garantit l'immutabilité et la stabilité du code.

De plus, il est possible d'ajouter ou de soustraire d'autres unités de temps que les jours, telles que les mois et les années. Par exemple:

```Kotlin
val today = LocalDate.now()
val futureDate = today.plusMonths(6).plusYears(2)
println(futureDate)
```

Cela affichera `2023-03-14` (si aujourd'hui est le 14 septembre 2021).

Enfin, pour afficher la date dans un format différent, vous pouvez utiliser la méthode `format` en spécifiant le format souhaité. Par exemple:

```Kotlin
val today = LocalDate.now()
val futureDate = today.plusDays(30)
val formattedDate = futureDate.format(DateTimeFormatter.ofPattern("dd MMMM yyyy"))
println(formattedDate)
```

Cela affichera `14 octobre 2021` (si aujourd'hui est le 14 septembre 2021).

# Voir aussi

- [Documentation de Kotlin sur la classe LocalDate](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.time/java.time.-local-date/index.html)
- [Documentation de Java sur la classe LocalDate](https://docs.oracle.com/javase/8/docs/api/java/time/LocalDate.html)
- [Tutoriel sur les dates et heures en Kotlin](https://www.baeldung.com/kotlin/java-time-dates)