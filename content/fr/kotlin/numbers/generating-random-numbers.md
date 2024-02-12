---
title:                "Génération de nombres aléatoires"
aliases: - /fr/kotlin/generating-random-numbers.md
date:                  2024-01-27T20:34:21.507568-07:00
model:                 gpt-4-0125-preview
simple_title:         "Génération de nombres aléatoires"

tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/kotlin/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?

Générer des nombres aléatoires en programmation consiste à créer des nombres qui ne présentent aucun schéma prévisible. Les programmeurs font cela pour diverses raisons, y compris les simulations, les tests d'algorithme, les jeux et les applications de sécurité, où l'imprévisibilité est la clé pour obtenir des résultats réalistes ou sécurisés.

## Comment faire :

Kotlin offre une manière simple de générer des nombres aléatoires grâce à sa bibliothèque standard. Voici comment vous pouvez générer différents types de valeurs aléatoires :

### Générer un Entier Aléatoire

Pour générer un entier aléatoire dans une plage spécifique :

```kotlin
import kotlin.random.Random

fun main() {
    val randomNumber = Random.nextInt(1, 100) // Génère un nombre aléatoire entre 1 et 99
    println(randomNumber)
}
```

### Générer un Double Aléatoire

De manière similaire, pour générer un double aléatoire :

```kotlin
import kotlin.random.Random

fun main() {
    val randomDouble = Random.nextDouble(1.0, 10.0) // Génère un double aléatoire entre 1.0 et 10.0
    println(randomDouble)
}
```

### Générer un Booléen Aléatoire

Pour générer une valeur booléenne aléatoire :

```kotlin
import kotlin.random.Random

fun main() {
    val randomBoolean = Random.nextBoolean() // Génère true ou false de manière aléatoire
    println(randomBoolean)
}
```

### Utiliser un Seed pour des Résultats Reproductibles

Dans les cas où vous avez besoin de séquences reproductibles de nombres aléatoires (par exemple, lors des tests), vous pouvez initialiser le générateur de nombres aléatoires avec une valeur de départ (seed) :

```kotlin
import kotlin.random.Random

fun main() {
    val seed = 12345L
    val random = Random(seed)
    val randomNumber = random.nextInt(1, 100)
    println(randomNumber)
}
```

## Plongée Profonde

L'approche de la bibliothèque standard Kotlin pour générer des nombres aléatoires tire parti de `java.util.Random` de Java en coulisse, assurant un équilibre entre facilité d'utilisation et performance. Cependant, il est crucial de noter que ces méthodes génèrent des nombres pseudo-aléatoires, ce qui signifie que les nombres semblent aléatoires mais sont générés à l'aide d'un processus déterministe.

Pour la plupart des applications, la randomicité fournie par la classe `Random` de Kotlin est suffisante. Cependant, pour des applications plus sensibles à la sécurité, comme la cryptographie, où la qualité de la randomicité est primordiale, on devrait envisager d'utiliser `java.security.SecureRandom` à la place. SecureRandom est spécifiquement conçu pour les opérations cryptographiques, fournissant une qualité de randomicité supérieure, bien qu'avec un compromis potentiel en termes de performance.

Kotlin ne réinvente pas la roue mais propose une API conviviale Kotlin par-dessus les mécanismes de génération de nombres aléatoires de Java, la rendant plus idiomatique et concise à utiliser dans les projets Kotlin. Comme toujours, lors de l'utilisation de l'aléatoire, les programmeurs devraient soigneusement considérer le cas d'usage pour choisir l'outil le plus approprié pour la tâche.
