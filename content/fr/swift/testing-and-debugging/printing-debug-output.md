---
date: 2024-01-20 17:53:17.821961-07:00
description: "Comment faire : Initialement, le d\xE9bogage se faisait avec des points\
  \ d\u2019arr\xEAt et la v\xE9rification manuelle des \xE9tats. L'arriv\xE9e des\
  \ fonctions d'affichage\u2026"
lastmod: '2024-04-05T21:53:59.636084-06:00'
model: gpt-4-1106-preview
summary: "Initialement, le d\xE9bogage se faisait avec des points d\u2019arr\xEAt\
  \ et la v\xE9rification manuelle des \xE9tats."
title: "Affichage des sorties de d\xE9bogage"
weight: 33
---

## Comment faire :
```Swift
// Impression simple de texte
print("Ceci est un message de debug.")

// Impression de variables avec interpolation de chaîne
var score = 42
print("Le score actuel est \(score)")

// Affichage conditionnel avec print en mode débogage
#if DEBUG
print("Ce message s'affiche uniquement en mode débogage.")
#endif

/* Exemple de sortie de console :
Ceci est un message de debug.
Le score actuel est 42
Ce message s'affiche uniquement en mode débogage.
*/
```

## Plongée en profondeur
Initialement, le débogage se faisait avec des points d’arrêt et la vérification manuelle des états. L'arrivée des fonctions d'affichage pour débogage a simplifié cette tâche. En Swift, `print()` est souvent utilisée, mais il existe d'autres outils comme `debugPrint()` qui donne un format plus détaillé, idéal pour les types complexes. `os_log` et `NSLog`, plus anciens, sont aussi des options, surtout utiles pour des logs système.

La fonction `print()` vient avec des paramètres pour plus de contrôle, par exemple `terminator` et `separator` qui permettent de personnaliser la fin de ligne et la séparation des valeurs. Utiliser `#if DEBUG` permet d’éviter que des messages de debug ne se retrouvent dans la version finale de l'application.

## Voir Aussi
- Documentation officielle de Swift sur `print()`: [Swift.org - Print](https://developer.apple.com/documentation/swift/1541053-print)
- Apple Developer Documentation sur le logging: [Unified Logging and Activity Tracing](https://developer.apple.com/documentation/os/logging)
