---
title:                "Convertir une date en chaîne de caractères"
html_title:           "Kotlin: Convertir une date en chaîne de caractères"
simple_title:         "Convertir une date en chaîne de caractères"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/kotlin/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Pourquoi et Pour quoi?

Convertir une date en chaîne de caractères est une tâche courante pour les programmeurs, car cela leur permet de représenter visuellement une date dans un format facile à lire pour les utilisateurs. Cela peut être utile pour diverses raisons, telles que la présentation de données dans un rapport ou l'affichage d'une date sur une interface utilisateur.

## Comment faire:

```Kotlin
// Kotlin code pour convertir une date en chaîne de caractères
val date = Date() // initialise la date à la date et heure actuelles
val format = SimpleDateFormat("dd/MM/yyyy") // initialise un format de date
val dateEnChaine = format.format(date) // utilise la méthode format pour convertir la date en chaîne de caractères
println(dateEnChaine) // imprime la date sous forme de chaîne de caractères dans la console (ex: "13/10/2021")
```

## Plongée profonde:

Contrairement à d'autres langages de programmation, Kotlin utilise une classe moderne pour représenter les dates, appelée ```java.util.Date```. Cette classe contient diverses méthodes pour gérer les dates, notamment ```format()``` qui est utilisée pour convertir la date en chaîne de caractères. De plus, il existe des alternatives telles que la classe ```LocalDate``` qui offre une approche plus moderne pour travailler avec les dates.

## Voir aussi:

- [Documentation officielle de Kotlin sur les dates et les heures](https://kotlinlang.org/docs/datetime.html)
- [Tutorial sur les dates et les heures en Kotlin](https://www.baeldung.com/kotlin/dates)
- [Comparaison entre les classes de dates en Java et en Kotlin](https://medium.com/tunaiku-tech/differences-between-java-dates-and-kotlin-dates-40f36b8fda72)