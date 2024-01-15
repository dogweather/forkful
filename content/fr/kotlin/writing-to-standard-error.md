---
title:                "Écrire sur le flux d'erreur standard"
html_title:           "Kotlin: Écrire sur le flux d'erreur standard"
simple_title:         "Écrire sur le flux d'erreur standard"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/kotlin/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Pourquoi

Écrire sur la sortie d'erreur standard est utile lorsque vous souhaitez afficher des informations de débogage sur votre programme. Cela peut vous aider à trouver et à résoudre les erreurs plus facilement.

## Comment faire

Voici comment écrire sur la sortie d'erreur standard en utilisant Kotlin :

```Kotlin
fun main() {
  // Code de votre programme
  println("Ceci est un message sur la sortie standard")
  System.err.println("Ceci est un message sur la sortie d'erreur standard")
}
```

La première ligne de code crée une fonction principale tandis que la deuxième ligne utilise la fonction prédéfinie `println()` pour afficher le message sur la sortie standard. La troisième ligne utilise la fonction `println()` de l'objet `System.err` pour écrire le message sur la sortie d'erreur standard. Le résultat de ces lignes de code sera :

```
Ceci est un message sur la sortie standard
Ceci est un message sur la sortie d'erreur standard
```

## Plongez plus profondément

Pour mieux comprendre comment écrire sur la sortie d'erreur standard en Kotlin, voici quelques points à connaître :

- En Kotlin, la classe `System` est utilisée pour accéder à la sortie standard et à la sortie d'erreur standard.
- La fonction `println()` est une méthode de la classe `PrintStream` qui est utilisée pour écrire sur la sortie standard.
- La fonction `println()` de l'objet `System.err` est une méthode de la classe `PrintStream` qui est utilisée pour écrire sur la sortie d'erreur standard.

Ces informations peuvent sembler techniques, mais elles sont importantes pour comprendre comment écrire efficacement sur la sortie d'erreur standard en utilisant Kotlin.

## Voir aussi

- [Documentation officielle de Kotlin](https://kotlinlang.org/docs/home.html)
- [Tutoriels de programmation Kotlin](https://www.programiz.com/kotlin-programming)
- [Guide de débogage en Kotlin](https://www.google.com/search?q=kotlin+debugging+guide)