---
title:                "Extraction de sous-chaînes"
html_title:           "Arduino: Extraction de sous-chaînes"
simple_title:         "Extraction de sous-chaînes"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/kotlin/extracting-substrings.md"
---

{{< edit_this_page >}}

## Qu'est-ce que c'est et pourquoi?

L'extraction de sous-chaînes est l'action de retirer une partie spécifique d'une chaîne de caractères. Les programmeurs le font pour récupérer et manipuler des informations spécifiques dans des séquences de texte plus longues.

## Comment faire :

Voici des exemples de la manière de le faire dans Kotlin:

```Kotlin
val chaine = "Apprenez Kotlin facilement"
val sousChaine = chaine.substring(9, 15)
println(sousChaine) //Output: Kotlin
```
Dans l'exemple ci-dessus, nous avons déclaré une chaîne `"Apprenez Kotlin facilement"` et nous avons utilisé la méthode `.substring(startIndex, endIndex)` pour extraire la sous-chaîne "Kotlin".

```Kotlin
val chaine = "Kotlin est cool"
val debut = chaine.substring(0, 6)
val fin = chaine.substring(10)
println(debut) //Output: Kotlin
println(fin) //Output: cool
```
Ici, nous voyons deux usages de .substring(). L'une pour extraire une sous-chaîne à partir d'un index de début jusqu'à la fin, l'autre à partir du début de la chaîne à un index de fin spécifique.

## Exploration en profondeur

Historiquement, l'extraction de sous-chaînes est une opération courante dans de nombreux langages de programmation. En Kotlin, la méthode `.substring()` est basée sur celle de Java, mais elle est simplifiée pour un usage plus intuitif.

Il existe différentes manières d'extraire des sous-chaînes en Kotlin. Pour plus de contrôle, nous pouvons utiliser `.substringBefore(delimiter)`, `.substringAfter(delimiter)` ou `.substringBetween(delimiter1, delimiter2)`.

```Kotlin
val chaine = "Apprenez-Kotlin-facilement"
val sousChaine = chaine.substringBefore("-")
println(sousChaine) //Output: Apprenez
```

La méthode `.substring()` effectue une copie de la portion de la chaîne originale, ce qui peut avoir un impact sur les performances pour de très longues chaînes. Cependant, pour la plupart des applications, cette différence de performance est négligeable.

## Voir aussi

Pour plus d'informations sur les string en Kotlin, consultez ces sources :

1. [Documentation Officielle Kotlin- Strings](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin/-string/)
3. [Medium - Exploring Strings in Kotlin](https://medium.com/androiddevelopers/exploring-kotlins-hidden-costs-part-1-fbb9935d9b62)

Ces ressources couvrent bien les différentes méthodes pour manipuler les chaînes de caractères en Kotlin et vous donneront une meilleure compréhension de comment utiliser efficacement ces méthodes.