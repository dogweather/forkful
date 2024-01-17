---
title:                "Extraction de sous-chaînes"
html_title:           "Kotlin: Extraction de sous-chaînes"
simple_title:         "Extraction de sous-chaînes"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/kotlin/extracting-substrings.md"
---

{{< edit_this_page >}}

# Qu'est-ce que c'est et pourquoi le faire?

Extraire des sous-chaînes de caractères est une technique courante en programmation qui consiste à sélectionner un morceau spécifique d'une chaîne de caractères plus grande. Cela peut être utile pour manipuler ou utiliser des données spécifiques dans un programme.

# Comment faire:

Voici deux façons différentes d'extraire des sous-chaînes en utilisant Kotlin:

```Kotlin
// Exemple 1:
val str = "Bonjour le monde!"
println(str.substring(8)) // sortie: "le monde!"

// Exemple 2:
val str = "Hello, je suis un programme"
println(str.substring(7, 16)) // sortie: "je suis un"
```

# Zoom sur:

- Contexte historique: L'utilisation de sous-chaînes est apparue dès les débuts de la programmation, lorsque les ordinateurs étaient limités en termes de mémoire et de puissance de calcul. Le fait de pouvoir extraire uniquement les données dont on a besoin a grandement aidé à optimiser les performances.

- Alternatives: Bien qu'extraire des sous-chaînes soit une méthode efficace, certains programmeurs préfèrent utiliser des regex (expressions régulières) pour sélectionner des parties spécifiques d'une chaîne de caractères.

- Détails d'implémentation: En Kotlin, la fonction `substring()` prend soit un seul paramètre (l'index de départ à partir duquel extraire la sous-chaîne), soit deux paramètres (l'index de départ et l'index de fin). L'index de début est inclus dans la sous-chaîne, tandis que l'index de fin ne l'est pas.

# Voir aussi:

- La documentation officielle sur les sous-chaînes en Kotlin: https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/substring.html
- Un article sur l'utilisation des sous-chaînes en Java: https://www.baeldung.com/java-substring
- Des exemples pratiques d'utilisation de sous-chaînes en programmation: https://www.geeksforgeeks.org/g-fact-81/