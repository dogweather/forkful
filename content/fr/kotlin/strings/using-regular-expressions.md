---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:17:22.718891-07:00
description: "Comment faire : Pour v\xE9rifier si une cha\xEEne correspond \xE0 un\
  \ motif sp\xE9cifique en Kotlin, vous pouvez utiliser la m\xE9thode `matches` de\
  \ la classe `Regex`."
lastmod: '2024-03-13T22:44:57.726196-06:00'
model: gpt-4-0125-preview
summary: "Pour v\xE9rifier si une cha\xEEne correspond \xE0 un motif sp\xE9cifique\
  \ en Kotlin, vous pouvez utiliser la m\xE9thode `matches` de la classe `Regex`."
title: "Utilisation des expressions r\xE9guli\xE8res"
weight: 11
---

## Comment faire :


### Correspondance de base
Pour vérifier si une chaîne correspond à un motif spécifique en Kotlin, vous pouvez utiliser la méthode `matches` de la classe `Regex`.

```kotlin
val pattern = "kotlin".toRegex()
val input = "I love kotlin"
val result = pattern.containsMatchIn(input)

println(result)  // Sortie : true
```

### Trouver et extraire des parties de la chaîne
Si vous souhaitez trouver des parties d'une chaîne qui correspondent à un motif, Kotlin vous permet d'itérer sur toutes les correspondances :

```kotlin
val datePattern = "\\d{2}/\\d{2}/\\d{4}".toRegex()
val input = "La date d'aujourd'hui est le 07/09/2023."
val dates = datePattern.findAll(input)

for (date in dates) {
    println(date.value)
}
// Sortie : 07/09/2023
```

### Remplacer du texte
Remplacer des parties d'une chaîne qui correspondent à un motif est simple avec la fonction `replace` :

```kotlin
val input = "Nom d'utilisateur : user123"
val sanitizedInput = input.replace("\\d+".toRegex(), "XXX")

println(sanitizedInput)  // Sortie : Nom d'utilisateur : userXXX
```

### Fractionner des chaînes
Fractionner une chaîne en une liste, en utilisant un motif regex comme délimiteur :

```kotlin
val input = "1,2,3,4,5"
val numbers = input.split(",".toRegex())

println(numbers)  // Sortie : [1, 2, 3, 4, 5]
```

### Bibliothèques tierces : Kotest
[Kotest](https://github.com/kotest/kotest) est une bibliothèque de tests Kotlin populaire qui étend le support des regex intégré de Kotlin, particulièrement utile pour la validation dans les cas de test.

```kotlin
// En supposant que Kotest est ajouté à votre projet
import io.kotest.matchers.string.shouldMatch

val input = "kotlin@test.com"
input shouldMatch "\\S+@\\S+\\.com".toRegex()

// Cela passera le test si l'entrée correspond au motif de courriel.
```

En intégrant les expressions régulières dans vos applications Kotlin, vous pouvez effectuer un traitement de texte sophistiqué de manière efficace. Que vous validiez des entrées utilisateur, extrayiez des données ou transformiez des chaînes, les motifs regex offrent une solution robuste.
