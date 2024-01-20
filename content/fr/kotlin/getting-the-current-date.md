---
title:                "Obtenir la date actuelle"
html_title:           "Bash: Obtenir la date actuelle"
simple_title:         "Obtenir la date actuelle"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/kotlin/getting-the-current-date.md"
---

{{< edit_this_page >}}

# Kotlin: Comment Obtenir La Date Courante

## Qu'est-ce que c'est & Pourquoi?

Obtenir la date actuelle signifie obtenir l'information du jour présent dans le format souhaité. Cela est utilisé dans la programmation pour enregistrer les événements, la gestion des logs, la facturation et beaucoup d'autres usages.

## Comment Faire:

En Kotlin, vous pouvez obtenir la date actuelle rapidement et facilement avec l'API moderne "java.time". Voici un exemple:

```Kotlin
import java.time.LocalDate

fun main() {
    val dateCourante = LocalDate.now()
    println("La date d'aujourd'hui est : $dateCourante")
}
```

Cette opération va produire la sortie suivante:

```Kotlin
La date d'aujourd'hui est : 2022-03-29
```

## Regard en Profondeur

Historiquement, nous utilisions `java.util.Date` ou `java.util.Calendar` pour obtenir la date actuelle, mais ces classes étaient complexe à utiliser en raison de leur conception. Dans Java 8, une nouvelle API de date/heure a été introduite et comme Kotlin est entièrement interopérable avec Java, nous pouvons l'utiliser facilement.

Une alternative serait d'utiliser `java.util.Date` comme ceci:

```Kotlin
import java.util.Date

fun main() {
    val date = Date()
    println(date)
}
```

Mais avec `java.time.LocalDate`, vous n'avez pas à vous soucier des heures, minutes et secondes.

L'implémentation de `LocalDate.now()` utilise le calendrier du système (Système par défaut `Clock`) pour obtenir la date courante. Cela donne le contexte courant de la zone horaire.

## Voir Aussi

Pour en savoir plus sur le travail avec les dates et heures dans kotlin, consultez ces ressources utiles:

- Blog de Baeldung: [Guide to java.time](https://www.baeldung.com/java-8-date-time-intro)

Allez-y et explorez toutes les possibilités offertes par la gestion moderne des dates et des heures avec Kotlin et Java!