---
title:                "Suppression de caractères correspondant à un motif"
html_title:           "C: Suppression de caractères correspondant à un motif"
simple_title:         "Suppression de caractères correspondant à un motif"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/kotlin/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi?

Supprimer des caractères correspondant à un modèle de données dans la programmation est une méthode d'ajustement, de nettoyage ou de conformité des données. On le fait principalement pour faciliter le traitement de ces données ou pour respecter certaines règles spécifiques.

## Comment:

Voici un exemple simple utilisant une expression régulière pour supprimer tous les chiffres d'une chaîne dans Kotlin:

```Kotlin
val pattern = "[0-9]".toRegex()
val sentence = "Mon numéro est 1234567890"
val result = sentence.replace(pattern, "")
println(result) // Résultat: "Mon numéro est "
```

Cela affichera "Mon numéro est ", supprimant tous les chiffres de la chaîne.

## Plongée Profonde:

Comme toute autre fonction dans la programmation, la suppression de caractères correspondant à un motif a ses origines et alternatives. Historiquement, cela a été fait en utilisant des boucles et des conditions, parcourant chaque caractère de la chaîne.

Cependant, avec la croissance des expressions régulières, la suppression par modèle est devenue beaucoup plus facile et plus directe. Par exemple, en Kotlin, nous utilisons la méthode `.replace()`, qui peut être plus efficace, mais connaître les alternatives peut être utile pour certains cas spécifiques.

Gardez à l'esprit qu'il y a des nuances concernant l'utilisation des expressions régulières, par l'exemple l'ordre dans lequel les remplacements sont effectués peut affecter les résultats. En particulier, il faut être prudent lorsqu'on supprime des caractères à l'aide d'un modèle qui peut se chevaucher.

## Voir Aussi:

Visiter ces ressources pour plus d'informations:

- [Documentation officielle de Kotlin sur les expressions régulières](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/-regex/)
- [Une excellente explication des modèles d'expressions régulières](https://developer.mozilla.org/fr/docs/Web/JavaScript/Guide/Regular_Expressions)
- [Plus de détails sur le fonctionnement des chaînes de caractères en Kotlin](https://kotlinlang.org/docs/character-strings.html)