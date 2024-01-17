---
title:                "Ecrire sur la sortie standard"
html_title:           "Swift: Ecrire sur la sortie standard"
simple_title:         "Ecrire sur la sortie standard"
programming_language: "Swift"
category:             "Swift"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/swift/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Qu'est-ce que c'est et pourquoi?

Ecrire vers la sortie d'erreur standard - ou "stderr" - est une pratique courante chez les programmeurs pour afficher des messages de débogage et d'erreur lors de l'exécution d'un programme. Cela peut aider les développeurs à identifier et résoudre rapidement les problèmes dans leur code.

## Comment faire:

Voici comment écrire vers stderr en Swift:

```Swift
FileHandle.standardError.write("Erreur: Impossible de trouver le fichier spécifié")
```

Ce code écrira le message "Erreur: Impossible de trouver le fichier spécifié" vers stderr.

## Plongée en profondeur:

L'écriture vers stderr est utilisée depuis longtemps dans la programmation et est considérée comme une pratique standard. Une alternative à cela est d'utiliser la sortie standard, également appelée "stdout", mais cela peut entraîner un mélange de messages de débogage et de résultats de sortie, ce qui peut rendre le débogage plus difficile.

L'implémentation de l'écriture vers stderr peut varier selon le langage de programmation utilisé. En Swift, l'utilisation de la fonction `write` sur le `FileHandle` associé à `standardError` est la façon recommandée d'écrire vers stderr.

## Voir aussi:

Vous pouvez en savoir plus sur l'utilisation et les avantages de l'écriture vers stderr en consultant les ressources suivantes:

- [Documentation officielle de Swift sur l'utilisation de `FileHandle`](https://developer.apple.com/documentation/foundation/filehandle/)
- [Article sur la différence entre stdin, stdout et stderr](https://www.gnu.org/software/libc/manual/html_node/Streams.html)