---
title:                "Kotlin: Obtenir la date actuelle."
programming_language: "Kotlin"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/kotlin/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Pourquoi 

Saviez-vous que vous pouvez facilement obtenir la date actuelle dans vos programmes Kotlin ? Dans cet article, nous allons vous montrer comment le faire et pourquoi cela peut être utile pour votre code.

## Comment faire 

Pour obtenir la date actuelle dans Kotlin, vous pouvez utiliser la fonction `LocalDate.now()`. Cette fonction renvoie un objet `LocalDate` qui représente la date actuelle dans le fuseau horaire du système. Voici un exemple de code :

```Kotlin
val currentDate = LocalDate.now()
println("La date actuelle est : $currentDate")
```

Output : La date actuelle est : 2020-07-25

Vous pouvez également spécifier un fuseau horaire différent en utilisant la fonction `now(zoneId)`, où `zoneId` est un identifiant de fuseau horaire tel que "Europe/Paris". Voici un exemple :

```Kotlin
val currentDate = LocalDate.now(ZoneId.of("Europe/Paris"))
println("La date actuelle en France est : $currentDate")
```

Output : La date actuelle en France est : 2020-07-25

## Analyse approfondie 

Maintenant que nous savons comment obtenir la date actuelle en Kotlin, voyons un peu plus en détail comment cela fonctionne. La classe `LocalDate` fait partie de la bibliothèque standard de Kotlin et elle fait partie de la spécification Java Time. L'objet renvoyé par les fonctions `now()` et `now(zoneId)` est un objet immuable qui représente une date spécifique sans prendre en compte l'heure ou le fuseau horaire. Cela peut être utile si vous avez besoin de travailler avec des dates sans tenir compte de l'heure ou du fuseau horaire.

## Voir aussi 

- [Documentation officielle de LocalDate](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.time/-local-date/)
- [Guide Java Time pour les débutants](https://www.baeldung.com/java-time)
- [Tutoriel sur Kotlin pour les programmeurs Java](https://kotlinlang.org/docs/tutorials/kotlin-for-java-programmers.html)