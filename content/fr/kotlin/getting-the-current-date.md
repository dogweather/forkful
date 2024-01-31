---
title:                "Obtenir la date actuelle"
date:                  2024-01-20T15:15:33.201850-07:00
html_title:           "C: Obtenir la date actuelle"
simple_title:         "Obtenir la date actuelle"

category:             "Kotlin"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/kotlin/getting-the-current-date.md"
---

{{< edit_this_page >}}

## What & Why?
Pourquoi et comment obtenir la date actuelle en Kotlin ? C'est simple : on a souvent besoin de savoir "quand" dans nos applis – pour des logs, des timestamps ou juste afficher la date. C'est une opération de base mais cruciale.

## How to:
Kotlin rend l'obtention de la date actuelle assez facile avec la librairie `java.time`. Voici comment on fait :

```Kotlin
import java.time.LocalDate
import java.time.format.DateTimeFormatter

fun main() {
    val currentDate = LocalDate.now()
    println("La date actuelle est: $currentDate")

    // Pour afficher la date dans un autre format
    val formatter = DateTimeFormatter.ofPattern("dd/MM/yyyy")
    val formattedDate = currentDate.format(formatter)
    println("La date formatée est: $formattedDate")
}
```
Et voilà le résultat :
```
La date actuelle est: 2023-04-12
La date formatée est: 12/04/2023
```

## Deep Dive:
Avant `java.time`, introduit dans Java 8, on utilisait `java.util.Date` mais c'était moins intuitif et sûr. `java.time` est inspiré de Joda-Time et conçu pour être plus clair et immuable, ce qui facilite la manipulation des dates sans side-effects.

Il y a des alternatives : on peut utiliser `Calendar` pour les anciennes versions de Java ou des biblio externes si on veut. Mais sincèrement, `java.time` est tellement bien intégré et puissant qu'il vaut mieux l'adopter.

Concernant l'implémentation, `LocalDate.now()` utilise l'horloge système pour récupérer la date actuelle. Ça s'appuie sur le fuseau horaire par défaut, donc la date retournée sera différente suivant l'endroit où le code tourne.

## See Also:
- La doc officielle de `java.time`: https://docs.oracle.com/javase/8/docs/api/java/time/package-summary.html
- Pour comprendre les différences entre `java.time`, `java.util.Date` et `Calendar`: https://www.baeldung.com/java-8-date-time-intro
- La page Stack Overflow pour résoudre les problèmes courants avec les dates en Kotlin: https://stackoverflow.com/questions/tagged/kotlin+date
