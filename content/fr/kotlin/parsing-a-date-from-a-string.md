---
title:                "Analyser une date à partir d'une chaîne."
html_title:           "Kotlin: Analyser une date à partir d'une chaîne."
simple_title:         "Analyser une date à partir d'une chaîne."
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/kotlin/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## Qu'est-ce que c'est et pourquoi les programmeurs le font?
Parsing une date à partir d'une chaîne de caractères est le processus de conversion d'une date sous forme de texte en un objet de date pouvant être manipulé et utilisé dans un programme. Les programmeurs le font pour traiter et manipuler des données liées aux dates, telles que des informations de réservation, des rapports, des données financières, etc.

## Comment faire:
Voici un exemple simple de code en Kotlin montrant comment utiliser la bibliothèque standard pour parser une date à partir d'une chaîne de caractères:
```Kotlin
val dateString = "17/06/2021" // Date sous forme de texte
val dateFormat = "dd/MM/yyyy" // Format de la date en texte
val date = SimpleDateFormat(dateFormat).parse(dateString) // Conversion de la chaîne en objet de date
println(date) // Résultat: Thu Jun 17 00:00:00 CEST 2021
```

## Plongée en profondeur:
Historiquement, le parsing de date à partir d'une chaîne de caractères était un processus complexe et sujet aux erreurs, car cela impliquait des algorithmes de traitement de données complexes. Cependant, avec l'avènement des bibliothèques standard et des frameworks dédiés, les programmeurs ont un moyen plus simple et plus sûr de gérer les dates dans leurs programmes. Alternativement, certains programmeurs peuvent également utiliser des librairies tierces pour des fonctionnalités plus avancées, telles que la manipulation de fuseaux horaires et le support pour des calendriers spécifiques.

## Voir aussi:
- [Documentation officielle de Kotlin pour le parsing de date en chaînes de caractères](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/-simple-date-format/)
- [Bibliothèque Joda-Time pour une manipulation avancée des dates en Java et Kotlin](https://www.joda.org/joda-time/)