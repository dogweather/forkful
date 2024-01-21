---
title:                "Génération de nombres aléatoires"
date:                  2024-01-20T17:49:50.569833-07:00
model:                 gpt-4-1106-preview
simple_title:         "Génération de nombres aléatoires"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/kotlin/generating-random-numbers.md"
---

{{< edit_this_page >}}

## What & Why? (Quoi & Pourquoi ?)
Générer des nombres aléatoires est une partie essentielle de la programmation pour créer de l'incertitude ou simuler des événements. Les programmeurs utilisent l'aléatoire pour des jeux, des simulations, des tests, et bien d'autres applications où l'imprévisibilité est nécessaire.

## How to (Comment faire ?)
Pour générer des nombres aléatoires en Kotlin, on utilise `Random`. Voici quelques exemples :

```kotlin
import kotlin.random.Random

fun main() {
    val randomValue = Random.nextInt(0, 100) // Entre 0 (inclus) et 100 (exclus)
    println(randomValue)

    val randomDiceRoll = Random.nextInt(1, 7) // Pour simuler un lancer de dé
    println(randomDiceRoll)
    
    val randomBoolean = Random.nextBoolean() // Vrai ou faux au hasard
    println(randomBoolean)
    
    val randomDouble = Random.nextDouble(1.0, 10.0) // Double entre 1.0 et 10.0
    println(randomDouble)
}
```

Sortie échantillon (va varier à chaque exécution) :
```
42
6
true
3.58765
```

## Deep Dive (Plongée en profondeur)
Historiquement, générer des nombres "aléatoires" en informatique est plutôt pseudo-aléatoire car ils dépendent de ce qu'on appelle une graine (seed). C'est une valeur initiale à partir de laquelle la séquence de nombres est générée, donc, en théorie, prévisible si la graine est connue.

En Kotlin, la classe `Random` utilise par défaut la graine liée à l'horloge système; mais on peut aussi en définir une manuellement :

```kotlin
val seededRandom = Random(1234) // Graine fixe pour reproducibilité
val predictableValue = seededRandom.nextInt()
println(predictableValue)
```

Des alternatives? Oui, on a `ThreadLocalRandom` pour les environnements multithreads et `SecureRandom` pour la cryptographie qui nécessite plus de sécurité et moins de prévisibilité.

Pour implémenter une fonctionnalité précise nécessitant de l'aléatoire, considérez le contexte: besoin de performance, de reproductibilité, ou de sécurité? Choisissez le bon outil en conséquence.

## See Also (Voir également)
- Documentation Kotlin sur Random: [https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.random/-random/](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.random/-random/)
- Oracle Docs sur SecureRandom: [https://docs.oracle.com/javase/8/docs/api/java/security/SecureRandom.html](https://docs.oracle.com/javase/8/docs/api/java/security/SecureRandom.html)
- Guide sur les nombres aléatoires et la concurrence: [https://www.baeldung.com/java-thread-local-random](https://www.baeldung.com/java-thread-local-random)