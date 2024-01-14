---
title:                "Swift: Impression de la sortie de débogage"
simple_title:         "Impression de la sortie de débogage"
programming_language: "Swift"
category:             "Swift"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/swift/printing-debug-output.md"
---

{{< edit_this_page >}}

## Pourquoi

Imprimer la sortie de débogage est un moyen essentiel de comprendre le fonctionnement d'un programme Swift. Cela peut vous aider à détecter les erreurs, à comprendre le flux d'exécution et à vérifier les valeurs des variables à un moment donné. Cela peut également être utile pour diagnostiquer des problèmes lors de la collaboration avec d'autres développeurs.

## Comment faire

```Swift
// Un message simple
print("Bonjour, Swift !")

// Affiche une variable
let age = 25
print("Mon âge est de \(age)")

// Affiche plusieurs valeurs sur la même ligne en utilisant la concaténation de chaînes
print("J'ai \(age) ans et j'apprends Swift depuis \(2) ans")
```

La méthode la plus courante pour imprimer la sortie de débogage est d'utiliser la fonction `print()`. Cette fonction prend en paramètre une ou plusieurs valeurs à imprimer et les affiche dans la console de débogage. Vous pouvez également utiliser la concaténation de chaînes ou l'interpolation de chaînes pour afficher plusieurs valeurs dans un seul message.

## Approfondissement

En plus de simplement afficher des valeurs dans la console, vous pouvez également personnaliser la sortie de débogage en utilisant l'option `separator` pour définir le délimiteur entre les valeurs et l'option `terminator` pour définir ce qui doit être affiché à la fin de la ligne.

De plus, vous pouvez utiliser des outils de débogage plus avancés tels que Xcode ou des bibliothèques tierces telles que CocoaLumberjack pour une meilleure gestion et personnalisation de la sortie de débogage.

## Voir aussi
- [Documentation officielle sur la fonction print() en Swift](https://docs.swift.org/swift-book/ReferenceManual/Statements.html#grammar_print-statement)
- [Guide de débogage de Xcode](https://developer.apple.com/library/archive/documentation/ToolsLanguages/Conceptual/Xcode_Overview/DebugYourApp/DebugYourApp.html)
- [CocoaLumberjack](https://github.com/CocoaLumberjack/CocoaLumberjack)