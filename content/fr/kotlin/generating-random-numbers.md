---
title:                "Générer des nombres aléatoires"
html_title:           "Elixir: Générer des nombres aléatoires"
simple_title:         "Générer des nombres aléatoires"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/kotlin/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi?

Générer des nombres aléatoires en Kotlin revient à produire des nombres qui ne sont pas prévisibles, ce qui est utile pour la sécurité, les jeux, les tests et bien plus encore.

## Comment Faire:

Voici comment générer un chiffre aléatoire en utilisant la bibliothèque standard de Kotlin.

```Kotlin
val random = (1..100).random()
println(random)
```
Exécutez-le et vous obtiendrez un chiffre aléatoire entre 1 et 100.

## Plongée en Profondeur:

Historiquement, les méthodes de génération de nombres aléatoires en informatique ont évolué, passant des générateurs de congruence linéaire aux générateurs Mersenne Twister. En Kotlin, on utilise généralement la méthode `random()` sur une plage de nombres entiers.

Il existe également des alternatives. Par exemple, pour une génération cryptographiquement forte, vous pouvez utiliser `SecureRandom` de Java dans votre code Kotlin.

```Kotlin
import java.security.SecureRandom
val secureRandom = SecureRandom()
val number = secureRandom.nextInt(100) + 1
println(number)
```

En ce qui concerne la mise en œuvre, il faut savoir que `random()` utilise une graine basée sur l'heure système actuelle pour générer des nombres, ce qui peut rendre les nombres prévisibles dans certaines situations.

## Voir Aussi:
	
Pour en savoir plus sur la génération de nombres aléatoires dans Kotlin, consultez ces liens:

1. Documentation Kotlin sur les nombres aléatoires: [https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.random/-random/](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.random/-random/)
3. Sécurité numérique en Kotlin: [https://proandroiddev.com/securerandom-c33906d12179](https://proandroiddev.com/securerandom-c33906d12179)