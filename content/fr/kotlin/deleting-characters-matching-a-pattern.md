---
title:                "Suppression des caractères correspondant à un motif"
html_title:           "Kotlin: Suppression des caractères correspondant à un motif"
simple_title:         "Suppression des caractères correspondant à un motif"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/kotlin/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Pourquoi

Si vous travaillez avec des chaînes de caractères en Kotlin, il est possible que vous ayez besoin de supprimer certains caractères selon un motif spécifique. Cela peut être utile dans différentes situations, comme la validation de données utilisateur ou le nettoyage de textes avant leur traitement.

## Comment Faire

Pour supprimer des caractères correspondant à un modèle, vous pouvez utiliser la fonction `replace` en spécifiant une expression régulière comme motif de recherche et une chaîne vide comme remplacement. Par exemple:

```Kotlin
val texte = "10 banana, 20 apple, 30 orange"
val texteModifie = texte.replace(Regex("[^0-9]"), "")
println(texteModifie) // Résultat: 102030
```

Dans cet exemple, nous utilisons `[^\0-9]` comme motif de recherche, ce qui correspond à tous les caractères non numériques. La fonction `replace` va alors les remplacer par une chaîne vide, laissant uniquement les chiffres dans la chaîne modifiée.

Vous pouvez également spécifier une expression régulière plus complexe pour cibler des motifs spécifiques dans votre texte. Par exemple, si vous voulez supprimer tous les caractères de ponctuation dans une chaîne, vous pouvez utiliser `\p{Punct}` comme motif de recherche:

```Kotlin
val texte = "Hello, world!"
val texteModifie = texte.replace(Regex("\\p{Punct}"), "")
println(texteModifie) // Résultat: Helloworld
```

## Plongée en Profondeur

L'utilisation des expressions régulières en Kotlin peut sembler intimidante au départ, mais c'est un outil très puissant pour manipuler des chaînes de caractères avec précision. Kotlin prend en charge les expressions régulières en utilisant la classe `Regex` et vous permet de spécifier différents modificateurs pour affiner votre recherche.

Par exemple, vous pouvez utiliser le modificateur `ignoreCase` pour ignorer la casse des caractères dans votre recherche, ou `multiline` pour étendre votre motif de recherche sur plusieurs lignes.

De plus, Kotlin offre également la possibilité d'utiliser des noms de groupe dans vos motifs de recherche, ce qui vous permet de capturer certains éléments de votre chaîne pour les utiliser dans votre remplacement.

Pour en savoir plus sur les expressions régulières en Kotlin, vous pouvez consulter la documentation officielle ou des ressources en ligne telles que [ce tutoriel](https://www.geeksforgeeks.org/kotlin-regular-expressions/) ou [ce guide complet](https://regexone.com/references/kotlin).

## Voir Aussi

- [Documentation officielle de Kotlin sur les expressions régulières](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/java.util.regex.-regex/index.html)
- [Tutoriel de GeeksforGeeks sur les expressions régulières en Kotlin](https://www.geeksforgeeks.org/kotlin-regular-expressions/)
- [Guide complet de RegexOne sur les expressions régulières en Kotlin](https://regexone.com/references/kotlin)