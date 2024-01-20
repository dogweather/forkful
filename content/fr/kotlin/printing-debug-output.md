---
title:                "Imprimer la sortie de débogage"
html_title:           "Arduino: Imprimer la sortie de débogage"
simple_title:         "Imprimer la sortie de débogage"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/kotlin/printing-debug-output.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?

L'affichage des débogages, c'est l'art d'émettre temporairement des infos contextuelles pendant l'exécution du code. Les programmeurs le font pour comprendre les problèmes de logique ou les bugs.

## Comment faire :

Voici un exemple simple d'estrade dans Kotlin :

```Kotlin
fun main() {
   val nom = "Pièrre"
   println("Bonjour, $nom")
}
```

Cela affiche :

```
Bonjour, Pièrre
```

Notez que `println` est la méthode standard pour imprimer des sorties en Kotlin. Ça émet une ligne de texte et un saut de ligne.

## Exploration profonde :

Historiquement, l'émission de débogage est ancestrale, utilisée depuis les premiers jours de la programmation. Elle reste une méthode simple et directe pour voir ce qui se passe au sein d'un programme.

En termes d'alternatives à Kotlin, des outils tels que `print` et `printf` existent pour des sorties plus spécifiques. `print` est similaire à `println` mais sans le saut de ligne final. `printf`, de son côté, offre un formatage plus sophistiqué.

Côté mise en œuvre, les méthodes `println`, `print` et `printf` sont toutes des membres du package standard `kotlin.io`. Ils agissent sur le flux de sortie standard (`System.out`).

## À voir également :

- [En savoir plus sur `kotlin.io`](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.io/)