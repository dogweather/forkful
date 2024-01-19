---
title:                "Analyser une date à partir d'une chaîne"
html_title:           "Clojure: Analyser une date à partir d'une chaîne"
simple_title:         "Analyser une date à partir d'une chaîne"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/kotlin/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## What & Why? (Quoi et Pourquoi ?)

Le parsing d'une date à partir d'une chaîne nous permet de convertir une date en format texte en un objet de date approprié. Cette pratique est courante parce qu'elle permet de manipuler, de formater et de calculer facilement la date et l'heure.

## How to: (Comment ça marche :)

```Kotlin
import java.time.LocalDate
import java.time.format.DateTimeFormatter
...
val dateString = "2024-12-31"
val formatter = DateTimeFormatter.ofPattern("yyyy-MM-dd")
val dateObject = LocalDate.parse(dateString, formatter)
println(dateObject)
```

La sortie sera: `2024-12-31`

## Deep Dive (Plongée Profonde)

Historiquement, le parsing des dates a toujours été une partie essentielle de la programmation car les dates sont souvent stockées et transférées en tant que chaînes de caractères. 

Une alternative à la méthode ci-dessus est l'utilisation de la méthode SimpleDateFormat, bien que cette dernière peut poser des problèmes de thread-safety.

Les spécificités d'implémentation de Kotlin rendent le parsing des dates à partir de chaînes particulièrement facile, grâce à l'excellente libraire `java.time`. Des formats supplémentaires peuvent également être obtenus en modifiant simplement le paramètre passé à `ofPattern`.

## See Also (Voir Aussi)

Voici quelques liens vers des informations complémentaires :

- Parsing et formatage des dates avec Java 8 : https://blog.ippon.fr/2013/12/10/parsing-et-formatting-des-dates-avec-java-8/
  
- Guide rapide de parsing des dates avec Kotlin : https://www.baeldung.com/kotlin/date-parsing

- Documentation officielle de Kotlin sur les chaînes et les caractères : https://kotlinlang.org/docs/strings.html