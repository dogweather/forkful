---
title:                "Kotlin: Trouver la longueur d'une chaîne."
simple_title:         "Trouver la longueur d'une chaîne."
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/kotlin/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

# Pourquoi

Il est essentiel de connaître la longueur d'une chaîne de caractères afin de pouvoir la manipuler correctement dans un programme Kotlin. Cela peut être utile lors de la validation des données d'un utilisateur ou dans d'autres opérations de traitement de données. Dans cet article, nous allons explorer comment trouver la longueur d'une chaîne de caractères en utilisant Kotlin.

# Comment faire

Pour trouver la longueur d'une chaîne de caractères en Kotlin, nous pouvons utiliser la méthode `length()` sur une variable de type `String` comme suit :

```Kotlin
val texte = "Bonjour le monde"
println(texte.length()) // Affiche "16" 
```

Nous pouvons également utiliser la propriété `length` sur une chaîne de caractères pour obtenir la même valeur :

```Kotlin
val autreTexte: String = "Au revoir"
println(autreTexte.length) // Affiche "9"
```

Une autre façon de trouver la longueur d'une chaîne de caractères en Kotlin est d'utiliser la fonction `count()` avec une expression régulière :

```Kotlin
val phrase = "Je suis un programmeur Kotlin"
println(phrase.count()) // Affiche "27"
println(phrase.count {it == 'e'}) // Affiche "3"
```

Il est important de noter que la méthode `count()` compte les caractères et non les éléments de la chaîne, donc les caractères spéciaux et les accents compteront également.

# Approfondissement

En Kotlin, les chaînes de caractères sont des objets plutôt que des valeurs primitives, ce qui signifie qu'elles ont des méthodes et des propriétés utiles. La méthode `length()` est également disponible pour les tableaux de caractères et les listes de caractères. De plus, la fonction `count()` peut prendre un paramètre prédéfini pour ignorer les majuscules et les minuscules lors du comptage des caractères.

# Voir aussi

Pour en savoir plus sur la manipulation des chaînes de caractères en Kotlin, jetez un œil aux ressources suivantes :

- [Documentation Kotlin sur les chaînes de caractères](https://kotlinlang.org/docs/reference/strings.html)
- [Tutoriel de programmation Kotlin pour débutants](https://www.tutorialspoint.com/kotlin/index.htm)
- [Chaînes de caractères en Kotlin : comment concaténer et remplacer des chaînes](https://www.baeldung.com/kotlin/string-concat-replace)