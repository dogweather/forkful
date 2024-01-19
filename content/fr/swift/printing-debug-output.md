---
title:                "Imprimer la sortie de débogage"
html_title:           "Arduino: Imprimer la sortie de débogage"
simple_title:         "Imprimer la sortie de débogage"
programming_language: "Swift"
category:             "Swift"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/swift/printing-debug-output.md"
---

{{< edit_this_page >}}

## Qu'est-ce que c'est & Pourquoi ?
L'affichage de debug est une méthode pour visualiser les messages d'erreur dans le console. Les programmeurs l'utilisent pour identifier et corriger les erreurs dans le code.

## Comment faire :
En Swift, on utilise la fonction `print()` pour afficher le debug. 

```Swift
var bugFinder = "Erreur inconnue"
print("Le bug est :", bugFinder)
```

Cela affichera: Le bug est : Erreur inconnue

On peut aussi afficher plusieurs éléments en une seule commande :

```Swift
var codeLine = 43
print("Le bug est :", bugFinder, "à la ligne", codeLine)
```
Ce code affichera : Le bug est : Erreur inconnue à la ligne 43

## Approfondissement
Swift a modernisé le concept de l'affichage de debug, qu'on trouve dans les langues historiquement importantes comme C.

Alternativement, on peut utiliser la fonction `debugPrint()`, qui fournit plus de détails que `print()`. Par exemple,

```Swift
debugPrint("Le bug est :", bugFinder)
```
Cette commande montre aussi le fichier, la date, l'heure, et la ligne de l'erreur.

Swift convertit nos données en chaînes de caractères avant de l'imprimer. Si notre donnée est un objet, Swift appelle sa méthode `description` pour la convertir.

## Voir aussi :
Pour une compréhension plus approfondie et des exemples détaillés sur l'impression de debug, reportez-vous à :
- [Official Swift Documentation](https://docs.swift.org/swift-book/LanguageGuide/BasicOperators.html)
- [StackOverflow Discussions on Swift Debugging](https://stackoverflow.com/questions/tagged/debugging+swift)
- [RayWenderlich Tutorial on Debugging](https://www.raywenderlich.com/105-debugging-with-xcode-10-console-breakpoints-print-statements)