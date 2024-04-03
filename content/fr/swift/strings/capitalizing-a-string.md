---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:06:30.601140-07:00
description: "Comment faire : Les structures `String` de Swift viennent avec quelques\
  \ m\xE9thodes int\xE9gr\xE9es pour manipuler la casse des cha\xEEnes. Voici quelques\
  \ approches\u2026"
lastmod: '2024-03-13T22:44:58.196721-06:00'
model: gpt-4-0125-preview
summary: "Les structures `String` de Swift viennent avec quelques m\xE9thodes int\xE9\
  gr\xE9es pour manipuler la casse des cha\xEEnes."
title: "Mettre en majuscule une cha\xEEne"
weight: 2
---

## Comment faire :
Les structures `String` de Swift viennent avec quelques méthodes intégrées pour manipuler la casse des chaînes. Voici quelques approches pour capitaliser les chaînes en Swift, y compris l'utilisation de méthodes standards et de bibliothèques tierces si nécessaire.

### Utiliser les méthodes intégrées
Pour capitaliser la première lettre d'une chaîne et mettre le reste en minuscule :

```swift
let myString = "hello, world"
let capitalizedString = myString.prefix(1).uppercased() + myString.dropFirst().lowercased()
print(capitalizedString) // Sortie : "Hello, world"
```

Pour capitaliser la première lettre de chaque mot dans une phrase, vous pouvez utiliser la propriété `capitalized` :

```swift
let sentence = "hello, world"
let capitalizedSentence = sentence.capitalized
print(capitalizedSentence) // Sortie : "Hello, World"
```

### Utiliser une bibliothèque tierce
Bien que la bibliothèque standard de Swift soit assez complète, certains formats de capitalisation spécifiques peuvent nécessiter des opérations plus complexes ou être simplifiés en utilisant des bibliothèques tierces. L'une des bibliothèques populaires pour la manipulation des chaînes est SwiftRichString. (Note : Assurez-vous toujours d'inclure les bibliothèques tierces via Swift Package Manager, CocoaPods ou Carthage, et de les importer dans votre fichier.)

D'abord, vous auriez besoin d'ajouter `SwiftRichString` à votre projet. Une fois installé, vous pouvez l'utiliser pour effectuer diverses opérations sur les chaînes, y compris les besoins spécifiques en matière de capitalisation. Cependant, à ce jour, les méthodes intégrées de Swift couvrent adéquatement la plupart des cas d'utilisation de la capitalisation sans avoir besoin de bibliothèques externes juste pour capitaliser des chaînes.

Référez-vous toujours à la documentation la plus récente de la bibliothèque pour tout mise à jour ou changement dans les méthodes.
