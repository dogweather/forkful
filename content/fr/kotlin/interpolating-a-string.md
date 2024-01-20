---
title:                "Interpolation d'une chaîne de caractères"
html_title:           "Ruby: Interpolation d'une chaîne de caractères"
simple_title:         "Interpolation d'une chaîne de caractères"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/kotlin/interpolating-a-string.md"
---

{{< edit_this_page >}}

## Qu'est-ce et Pourquoi ?

L'interpolation de chaînes en Kotlin est la technique d'insérer des variables au sein d'une chaîne de caractères. Elle rend le code non seulement plus lisible, mais aussi plus efficace lorsqu'on souhaite concaténer des variables et des chaînes de caractères.

## Comment faire :

Utiliser le dollar (`$`) pour insérer une variable dans une chaîne. Si vous souhaitez inclure une expression entière, encadrez-la avec des accolades (`{}`). 

En Kotlin, cela ressemble à ça :

```Kotlin
val nom = "Pierre"
println("Bonjour, $nom")  // Affiche : Bonjour, Pierre

val age = 30
println("Bonjour, $nom. Vous avez $age ans.")  // Affiche : Bonjour, Pierre. Vous avez 30 ans.

println("Dans dix ans, vous aurez ${age + 10} ans.")  // Affiche : Dans dix ans, vous aurez 40 ans.
```

## Plongée profonde :

Historiquement, l'interpolation de chaînes a été introduite pour la première fois dans le langage de programmation ALGOL 68. C'est une fonctionnalité que l'on retrouve dans beaucoup de langages modernes dont Kotlin, bien sûr.

Il existe de nombreuses manières alternativas de concaténer des chaînes et des variables, par exemple avec l'opération `+`. Cependant, ce n'est pas recommandé car il peut créer de nombreux objets String inutiles, surtout dans les boucles.

Par exemple, évitez cela :

```Kotlin
val nom = "Pierre"
val age = 30
println("Bonjour, " + nom + ". Vous avez " + age + " ans.")
```

En pratique, l'interpolation de chaînes en Kotlin est mise en œuvre en traduisant les expressions `${expression}` en appels à `expression.toString()` lors de la compilation. 

## Voir aussi :

Pour en savoir plus, considérez ces ressources :

1. [Documentation officielle sur l'interpolation des chaînes en Kotlin](https://kotlinlang.org/docs/basic-syntax.html#string-templates)
3. [Discussion StackOverflow sur l'interpolation des chaînes en Kotlin](https://stackoverflow.com/questions/46450220/when-should-i-use-string-interpolation-in-kotlin)