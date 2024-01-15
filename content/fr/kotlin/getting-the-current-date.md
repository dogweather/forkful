---
title:                "Obtenir la date actuelle."
html_title:           "Kotlin: Obtenir la date actuelle."
simple_title:         "Obtenir la date actuelle."
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/kotlin/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Pourquoi

Vous êtes probablement ici parce que vous souhaitez utiliser Kotlin pour obtenir la date actuelle. Et devinez quoi ? C'est très facile à faire ! La plupart des langages de programmation modernes ont une fonction intégrée pour cela et Kotlin ne fait pas exception.

## Comment faire

```Kotlin
// Importez la classe java.util.Date
import java.util.Date

// Utilisez la fonction `Date()` pour créer un objet de type "Date"
val date = Date()

// Utilisez la fonction `toString()` pour afficher la date actuelle
println(date.toString())

// Vous pouvez également formater la date selon vos préférences
val format = SimpleDateFormat("dd-MM-yyyy") 
println(format.format(date))
```

**Sortie :** 
```
Sat Jan 09 16:11:26 EST 2021
09-01-2021
```

## Plongez plus profondément

La classe `Date` de Kotlin provient en fait de la bibliothèque standard Java. Elle contient des méthodes utiles pour manipuler les dates et les heures, comme `compareTo()`, `before()`, `after()`, etc.

De plus, Kotlin dispose d'une autre classe appelée `LocalDateTime` qui offre une alternative plus moderne au type `Date`. Elle prend en compte les fuseaux horaires et les horloges avec précision, ce qui peut être utile pour les applications internationales.

## Voir aussi

- [Java Date and Time](https://docs.oracle.com/javase/8/docs/api/java/time/package-summary.html) (Documentation officielle Java)
- [Working with Dates in Kotlin](https://www.baeldung.com/kotlin/dates) (Tutoriel Baeldung)