---
title:                "Ecrire sur l'erreur standard"
html_title:           "Swift: Ecrire sur l'erreur standard"
simple_title:         "Ecrire sur l'erreur standard"
programming_language: "Swift"
category:             "Swift"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/swift/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Pourquoi

Il peut être utile d'écrire vers la sortie d'erreur standard (stderr) lors de la programmation en Swift pour déboguer votre code et afficher des messages d'erreurs plus précis.

## Comment faire

Utilisez la fonction `print()` en ajoutant `"\n"` à la fin pour écrire vers stderr. Voici un exemple de code:

```Swift
print("Erreur: Impossible de se connecter au serveur\n", to: &stderr)
```
Voici ce que cela produirait en sortie:

```
Erreur: Impossible de se connecter au serveur
```

## Plongée en profondeur

La sortie d'erreur standard ou stderr est un flux de données dans lequel les erreurs et les avertissements sont généralement envoyés lors de l'exécution d'un programme. En l'utilisant pour écrire des messages d'erreurs, vous pouvez aider à localiser et résoudre plus facilement les problèmes dans votre code.

## Voir aussi

- [Documentation officielle de Swift sur la sortie d'erreur standard](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html#ID506)
- [Guide de débogage en Swift](https://medium.com/swift-programming/swift-guide-how-to-debug-4e035a60d5df)
- [Vidéo explicative sur la gestion des erreurs en Swift](https://www.youtube.com/watch?v=IVp1UuPh-i0)