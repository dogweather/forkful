---
title:                "Convertir une date en chaîne de caractères"
html_title:           "Gleam: Convertir une date en chaîne de caractères"
simple_title:         "Convertir une date en chaîne de caractères"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/kotlin/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Qu'est-ce et Pourquoi ?
Convertir une date en chaîne de caractères en Kotlin signifie transformer une instance de Date en une chaîne de texte formatée. Les programmeurs le font pour que les humains puissent lire et comprendre les dates, qui sont sinon stockées en format numérique non lisible.

## Comment faire :

Voici un exemple de code qui montre comment convertir une date en chaîne en Kotlin.

```Kotlin
import java.text.SimpleDateFormat
import java.util.Date

fun main() {
    val date = Date()
    val format = SimpleDateFormat("dd/MM/yyyy")
    val dateString = format.format(date)
    println(dateString)
}
```

Lors de l'exécution de ce code, vous devriez voir une sortie similaire à celle-ci :

```Kotlin
30/11/2023
```
Ce qui signifie que nous sommes le 30 Novembre 2023.

## Plongeons un peu plus profond :

Historiquement, les dates en informatique sont stockées sous forme de nombres pour une gestion de l'espace plus efficace et une facilité de manipulation. Pourtant, ce format n'est pas très lisible pour nous, humains. C'est pourquoi nous avons besoin de les convertir en chaînes de caractères.

Une alternative à SimpleDateFormat serait d'utiliser la classe LocalDateTime de Java 8, qui a une meilleure gestion des fuseaux horaires. 

L'implémentation du `format` en Kotlin est essentiellement identique à celle en Java, puisque Kotlin s'appuie sur les API Java. L'objectif de `format.format(date)` est de convertir notre objet `Date` en chaîne selon le format défini.

## Voir aussi :

Pour plus d'informations sur le formatage des dates en Java et Kotlin, consultez les liens ci-dessous :

- Documentation officielle sur SimpleDateFormat : https://docs.oracle.com/javase/8/docs/api/java/text/SimpleDateFormat.html
- Guide Kotlin sur Baeldung : https://www.baeldung.com/kotlin-date-time
- Tutoriel Java sur les dates et les heures : https://www.w3schools.com/java/java_date.asp