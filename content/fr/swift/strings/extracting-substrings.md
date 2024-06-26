---
date: 2024-01-20 17:46:28.573592-07:00
description: "How to: (Comment faire :) Historiquement, l'extraction de sous-cha\xEE\
  nes en Swift a \xE9volu\xE9 pour simplifier son interface et am\xE9liorer la s\xE9\
  curit\xE9 de type.\u2026"
lastmod: '2024-04-05T22:51:12.093755-06:00'
model: gpt-4-1106-preview
summary: "(Comment faire :) Historiquement, l'extraction de sous-cha\xEEnes en Swift\
  \ a \xE9volu\xE9 pour simplifier son interface et am\xE9liorer la s\xE9curit\xE9\
  \ de type."
title: "Extraction de sous-cha\xEEnes"
weight: 6
---

## How to: (Comment faire :)
```Swift
// Exemple de base pour extraire une sous-chaîne
let phrase = "Bonjour à tous, bienvenue dans le monde de Swift!"
let indexDebut = phrase.index(phrase.startIndex, offsetBy: 8)
let indexFin = phrase.index(phrase.startIndex, offsetBy: 21)
let sousChaine = phrase[indexDebut...indexFin]
print(sousChaine) // Affiche: "à tous, bienven"

// Utilisation de NSRange avec NSString
import Foundation
let nsPhrase = phrase as NSString
let nsSousChaine = nsPhrase.substring(with: NSRange(location: 8, length: 14))
print(nsSousChaine) // Affiche: "à tous, bienven"

// Extraire avec des méthodes de chaîne de haute niveau
if let range = phrase.range(of: "bienvenue") {
    let sousChaineHauteNiveau = phrase[range]
    print(sousChaineHauteNiveau) // Affiche: "bienvenue"
}
```

## Deep Dive (Plongée Profonde)
Historiquement, l'extraction de sous-chaînes en Swift a évolué pour simplifier son interface et améliorer la sécurité de type. Avant Swift 4, on utilisait des indices d'entier pour extraire des sous-chaînes ; maintenant, on préfère les `String.Index` pour éviter les erreurs d'exécution. Il existe aussi des alternatives comme `NSString` qui vient d'Objective-C, mais l'utilisation de méthodes natives Swift est généralement préférable pour la cohérence et la performance. Quand on travaille avec des sous-chaînes, on manipule en fait des vues sur la chaîne originale, sans copie, rendant l'opération plus efficace.

## See Also (Voir Aussi)
- [Documentation officielle Swift sur les Strings](https://developer.apple.com/documentation/swift/string)
- [Swift Book - Working with Strings](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html)
- [Ray Wenderlich - Swift String Cheat Sheet](https://www.raywenderlich.com/5539282-swift-string-cheat-sheet-and-quick-reference)
