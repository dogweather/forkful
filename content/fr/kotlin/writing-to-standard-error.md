---
title:                "Écrire dans l'erreur standard"
date:                  2024-01-19
html_title:           "Arduino: Écrire dans l'erreur standard"
simple_title:         "Écrire dans l'erreur standard"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/kotlin/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Quoi et Pourquoi ?
Écrire dans la sortie d'erreur standard (stderr) permet de séparer les messages d'erreur du flot de sortie principal (stdout). Les programmeurs l'utilisent pour signaler les problèmes sans perturber le reste de la sortie.

## Comment faire :
```kotlin
fun main() {
    println("Ceci est envoyé à la sortie standard.")
    System.err.println("Ceci est envoyé à la sortie d'erreur.")
}
```
Résultat possible:
```
Ceci est envoyé à la sortie standard.
Ceci est envoyé à la sortie d'erreur.
```

## Plongée en profondeur
Historiquement, séparer stderr de stdout permettait de gérer les messages d'erreur indépendamment. Des alternatives existent, comme utiliser des fichiers logs, mais stderr reste privilégié pour sa simplicité et son intégration avec des outils Unix. En Kotlin, `System.err` représente cette sortie d'erreur et fonctionne de la même manière que `System.out`.

## Voir également
- Documentation Kotlin sur les entrées/sorties standard : [https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.io/](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.io/)
