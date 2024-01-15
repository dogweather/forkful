---
title:                "Afficher les sorties de débogage"
html_title:           "Kotlin: Afficher les sorties de débogage"
simple_title:         "Afficher les sorties de débogage"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/kotlin/printing-debug-output.md"
---

{{< edit_this_page >}}

## Pourquoi

Vous êtes en train de coder avec Kotlin et vous rencontrez des problèmes dans votre code. Vous avez réussi à incorporer quelques fonctions et vous voulez maintenant les tester. L'impression de sorties de débogage peut vous aider à comprendre le comportement de votre code et à résoudre les problèmes plus rapidement.

## Comment faire

Les instructions suivantes vous montreront comment imprimer les sorties de débogage dans vos projets Kotlin :

```Kotlin
// Déclaration d'un message de débogage
val message = "Ceci est un message de débogage"

// Imprimer le message dans la console
println(message)

// Imprimer un message avec un argument
val count = 5
println("Il y a $count éléments dans la liste.")
```
```
Output:
Ceci est un message de débogage
Il y a 5 éléments dans la liste.
```
Vous pouvez également utiliser la fonction de débogage intégrée de Kotlin pour imprimer les valeurs de variables :

```Kotlin
val name = "Marie"
val age = 25
debug { "Le nom est $name et l'âge est $age" }
```
```
Output:
[app_name out] Le nom est Marie et l'âge est 25
```

## Deep Dive

L'impression de sorties de débogage est utile pour afficher des informations sur votre programme en cours d'exécution, y compris les valeurs des variables et les messages de débogage. Cela peut vous aider à identifier les erreurs dans votre code, à suivre l'exécution de votre algorithme et à comprendre la logique de votre programme. Vous pouvez également personnaliser vos messages de débogage en y incluant des expressions Kotlin, comme nous l'avons montré dans notre deuxième exemple.

## Voir aussi
- [Kotlin Official Documentation](https://kotlinlang.org/docs/reference/)
- [Debugging in Kotlin](https://www.raywenderlich.com/77235-debugging-on-android-studio-tutorial)