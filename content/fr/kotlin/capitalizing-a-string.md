---
title:                "Mettre une chaîne en majuscule."
html_title:           "Kotlin: Mettre une chaîne en majuscule."
simple_title:         "Mettre une chaîne en majuscule."
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/kotlin/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Qu'est-ce que c'est et pourquoi le faire?
Lorsqu'un programmeur capitalise une chaîne de caractères, il modifie la casse de la première lettre de chaque mot pour la rendre en majuscule. Cela peut être utile lors de l'affichage de noms ou de titres, afin de les rendre plus lisibles et cohérents. Les programmeurs le font généralement pour une question de convention et de bonnes pratiques de codage.

## Comment faire:
Un exemple simple de la méthode de capitalisation dans Kotlin peut être réalisé en utilisant la fonction ```capitalize()``` sur un objet de type String. Voici un exemple de code:
```
val str: String = "bonjour le monde"
println(str.capitalize())
```
La sortie de ce code serait "Bonjour le monde".

On peut également utiliser la fonction ```toUpperCase()``` pour mettre tous les caractères en majuscules, ou ```toLowerCase()``` pour les mettre en minuscules.

## Plongée en profondeur:
Historiquement, la casse des lettres a toujours eu de l'importance dans la programmation. Cela remonte aux anciens systèmes de codage tels que l'ASCII qui différenciait les majuscules et les minuscules. Bien que les conventions et les styles de codage aient évolué au fil du temps, il reste important de maintenir une certaine cohérence dans l'utilisation de la casse pour rendre le code plus facile à lire et à comprendre.

Alternativement, certains développeurs peuvent préférer utiliser la fonction ```replaceFirstChar``` pour capitaliser une chaîne sans avoir besoin d'une nouvelle chaîne.

Dans l'implémentation de la fonction ```capitalize()``` en Kotlin, la première lettre est mise en majuscule selon les règles linguistiques de la langue actuelle du système en cours d'exécution.

## À voir aussi:
Pour plus d'informations sur la manipulation des chaînes de caractères dans Kotlin, vous pouvez consulter la documentation officielle du langage ici: https://kotlinlang.org/docs/reference/basic-types.html#strings