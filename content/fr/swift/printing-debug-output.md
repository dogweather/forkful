---
title:                "Affichage des erreurs de débogage"
html_title:           "Swift: Affichage des erreurs de débogage"
simple_title:         "Affichage des erreurs de débogage"
programming_language: "Swift"
category:             "Swift"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/swift/printing-debug-output.md"
---

{{< edit_this_page >}}

## Pourquoi

Vous vous demandez peut-être pourquoi il serait nécessaire d'imprimer des informations de débogage dans votre code Swift. Eh bien, cela peut être utile lorsque vous rencontrez des erreurs et que vous avez besoin de mieux comprendre ce qui se passe dans votre programme.

## Comment

Pour imprimer des informations de débogage dans votre code Swift, utilisez la fonction `print()`. Voici un exemple de code avec un message d'erreur et une sortie de débogage :

```Swift
// Déclaration d'une variable avec une valeur incorrecte
var age = -10

// Affichage d'un message d'erreur
print("L'âge ne peut pas être un nombre négatif")

// Impression de l'âge pour le débogage
print("L'âge est de \(age)")
```

Cette sortie de débogage peut vous aider à comprendre pourquoi votre programme a rencontré une erreur, en vous montrant la valeur de la variable `age`.

## Plongée en profondeur

Il est important de noter que l'impression de débogage peut ralentir l'exécution de votre code, donc il est recommandé de l'utiliser uniquement pendant le processus de débogage et de le supprimer une fois que votre code fonctionne correctement. De plus, vous pouvez également utiliser des `print` statements pour afficher des messages d'état ou des valeurs de variables à différentes étapes de votre programme, pour vous aider à suivre son exécution.

## Voir aussi

- [Documentation officielle de Swift sur l'impression de débogage](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html#ID280)
- [Guide de débogage en utilisant l'impression dans Xcode](https://medium.com/@darylashton/print-debugging-swift-using-xcode-ea85174adb5c)
- [Vidéo expliquant l'utilisation de l'impression pour le débogage en Swift](https://www.youtube.com/watch?v=eLnJG3ruL5I)