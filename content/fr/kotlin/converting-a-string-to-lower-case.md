---
title:    "Kotlin: Transformer une chaîne de caractères en minuscules"
keywords: ["Kotlin"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/kotlin/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Pourquoi

Avant de plonger dans le sujet de la conversion d'une chaîne de caractères en minuscules en Kotlin, il est important de comprendre pourquoi cela peut être utile dans votre code. En convertissant une chaîne en minuscules, vous pouvez uniformiser le format des données et faciliter leur comparaison. Cela peut également être utile lorsque vous travaillez avec des données sensibles à la casse, comme des mots de passe.

Maintenant que vous avez une idée de l'utilité de cette opération, passons à la partie pratique.

## Comment faire

La conversion d'une chaîne de caractères en minuscules en Kotlin est très simple grâce à la fonction intégrée "toLowerCase()". Voyons un exemple de code pour mieux comprendre :

```Kotlin
val phrase = "Bonjour à TOUS les développeurs!"
val lowercasePhrase = phrase.toLowerCase()
println(lowercasePhrase)
```

Output : bonjour à tous les développeurs!

Comme vous pouvez le voir, la méthode "toLowerCase()" a été appliquée à la variable "phrase" et sa valeur a été affectée à la variable "lowercasePhrase". Le résultat est alors affiché en minuscules grâce à la fonction "println".

Vous pouvez également utiliser cette méthode directement sur une chaîne de caractères sans avoir à utiliser une variable supplémentaire :

```Kotlin
println("HELLO World!".toLowerCase())
```

Output : hello world!

Vous pouvez également utiliser cette méthode pour comparer deux chaînes de caractères sans vous soucier de la casse :

```Kotlin
val password = "MotDePasse"
val userInput = "motdepasse"
val lowercasePassword = password.toLowerCase()
if (lowercasePassword == userInput.toLowerCase()) {
    println("Mot de passe valide")
} else {
    println("Mot de passe invalide")
}
```

Output : Mot de passe valide

Cependant, il est important de noter que la méthode "toLowerCase()" ne fonctionne qu'avec les caractères ASCII. Si vous travaillez avec des caractères Unicode, vous devrez utiliser la fonction "CaseFormat" de la bibliothèque Google Guava.

## Plongée en profondeur

La méthode "toLowerCase()" en Kotlin fonctionne en transformant les caractères de la chaîne en minuscules, en fonction de leur valeur numérique de caractère Unicode. En d'autres termes, chaque caractère est converti en utilisant le tableau ASCII.

Par exemple, le caractère "A" (code ASCII 65) sera converti en "a" (code ASCII 97). De même, le caractère "Z" (code ASCII 90) sera converti en "z" (code ASCII 122).

Cependant, il est important de noter que cette méthode ne fonctionne pas pour les caractères multibytes ou les caractères accentués. Ces caractères peuvent avoir une valeur de caractère Unicode différente et ne seront pas convertis correctement en minuscules.

## Voir aussi

- La documentation officielle de Kotlin sur les chaînes de caractères : https://kotlinlang.org/docs/strings.html#using-string-transformation-functions
- La documentation officielle de la bibliothèque Google Guava : https://github.com/google/guava
- Un article sur la comparaison de chaînes en Kotlin : https://blog.kotlin-academy.com/string-comparison-in-kotlin-a133bdf790e3