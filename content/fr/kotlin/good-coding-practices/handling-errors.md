---
date: 2024-01-26 00:54:37.810763-07:00
description: "Comment faire : Kotlin fournit `try`, `catch`, `finally` et `throw`\
  \ pour g\xE9rer les erreurs. Voici comment les utiliser ."
lastmod: '2024-03-13T22:44:57.747883-06:00'
model: gpt-4-1106-preview
summary: "Kotlin fournit `try`, `catch`, `finally` et `throw` pour g\xE9rer les erreurs."
title: Gestion des erreurs
weight: 16
---

## Comment faire :
Kotlin fournit `try`, `catch`, `finally` et `throw` pour gérer les erreurs. Voici comment les utiliser :

```Kotlin
fun main() {
    val numerateur = 10
    val denominateur = 0

    try {
        val resultat = numerateur / denominateur
        println("Résultat : $resultat")
    } catch (e: ArithmeticException) {
        println("On ne peut pas diviser par zéro, l'ami.")
    } finally {
        println("Cela arrive quoi qu'il en soit.")
    }
}
```

Sortie :
```
On ne peut pas diviser par zéro, l'ami.
Cela arrive quoi qu'il en soit.
```

Si quelque chose tourne mal dans le bloc `try`, l'exécution se déplace vers `catch`. Il capture l'erreur spécifique lancée (l'`ArithmeticException` dans ce cas). Le block `finally` s'exécute après — quel que soit le résultat.

## Plongée en profondeur
Le bloc `try-catch` existe depuis les premiers jours de la programmation — c'est comme un filet de sécurité. Kotlin offre également `throw` pour jeter manuellement une exception dans l'arène, et il y a `finally` pour le code qui doit être exécuté — souvent des travaux de nettoyage.

Les alternatives comprennent le type `Result` et `try` de Kotlin en tant qu'expression.

```Kotlin
val resultat: Result<Int> = try {
    Result.success(numerateur / denominateur)
} catch (e: ArithmeticException) {
    Result.failure(e)
}
```
Cette approche retourne un objet `Result` — vous obtenez soit un succès, soit un échec sans le drame d'une exception non gérée.

L'implémentation en Kotlin est élégante car vous pouvez utiliser `try` comme une expression, ce qui signifie qu'elle retourne une valeur. Des choix comme ceux-ci rendent la gestion des erreurs en Kotlin assez polyvalente. Il s'agit de choisir le bon outil pour le travail, tout comme vous le feriez dans un atelier.

## Voir aussi
- Docs Kotlin sur les Exceptions : [Gestion des exceptions Kotlin](https://kotlinlang.org/docs/exception-handling.html)
- Docs sur le type `Result` de Kotlin : [Résultat Kotlin](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin/-result/)
- Effective Java, 3e édition, par Joshua Bloch — d'excellentes perspectives sur les exceptions, même si c'est spécifique à Java.
