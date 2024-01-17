---
title:                "Trouver la longueur d'une chaîne de caractères"
html_title:           "Kotlin: Trouver la longueur d'une chaîne de caractères"
simple_title:         "Trouver la longueur d'une chaîne de caractères"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/kotlin/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

Qu'est-ce que c'est et pourquoi le faire?

Trouver la longueur d'une chaîne est le processus de déterminer le nombre de caractères dans une chaîne de texte. Les programmeurs le font pour diverses raisons, telles que la validation de l'entrée utilisateur ou la manipulation d'une chaîne pour effectuer des opérations mathématiques.

Comment faire:

```Kotlin
var string = "Bonjour!"
println("La longueur de la chaîne est ${string.length}")
// Sortie: La longueur de la chaîne est 8
```

```Kotlin
var autreString = "Au revoir"
println("La longueur de la chaîne est ${autreString.length}")
// Sortie: La longueur de la chaîne est 9
```

Plongée en profondeur:

- Contexte historique: Trouver la longueur d'une chaîne est une tâche courante en programmation depuis les premiers langages de programmation.
- Alternatives: Dans certains langages, comme Java, il existe une méthode spécifique pour trouver la longueur d'une chaîne, tandis que d'autres langages, comme Python, utilisent une fonction intégrée pour cela.
- Détails d'implémentation: En Kotlin, la propriété "length" est disponible pour tous les objets de la classe "String", ce qui facilite la tâche aux programmeurs.

Voir aussi:

- Documentation Kotlin sur la propriété "length": https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/-string/length.html
- Différents moyens de trouver la longueur d'une chaîne en Java: https://www.geeksforgeeks.org/string-length-method-in-java/