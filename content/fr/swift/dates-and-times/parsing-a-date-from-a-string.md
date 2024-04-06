---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:15:29.388608-07:00
description: "Comment faire : La biblioth\xE8que standard de Swift, Foundation, fournit\
  \ `DateFormatter` pour convertir des cha\xEEnes en objets `Date` et vice versa.\
  \ Pour\u2026"
lastmod: '2024-03-13T22:44:58.233055-06:00'
model: gpt-4-0125-preview
summary: "La biblioth\xE8que standard de Swift, Foundation, fournit `DateFormatter`\
  \ pour convertir des cha\xEEnes en objets `Date` et vice versa."
title: "Analyser une date depuis une cha\xEEne de caract\xE8res"
weight: 30
---

## Comment faire :


### Utiliser `DateFormatter` de Foundation
La bibliothèque standard de Swift, Foundation, fournit `DateFormatter` pour convertir des chaînes en objets `Date` et vice versa. Pour analyser une date à partir d'une chaîne, vous spécifiez le format de date qui correspond à la chaîne, puis utilisez le formateur pour l'analyser.

```swift
import Foundation

let dateString = "2023-04-30"
let formatter = DateFormatter()
formatter.dateFormat = "yyyy-MM-dd"
if let date = formatter.date(from: dateString) {
    print("Date analysée : \(date)")
} else {
    print("Échec de l'analyse de la date")
}
// Exemple de sortie : Date analysée : 2023-04-29 22:00:00 +0000
```

Notez que la sortie peut varier en fonction de votre fuseau horaire.

### Utiliser ISO8601DateFormatter
Pour les formats de date ISO 8601, Swift propose un formateur spécialisé, `ISO8601DateFormatter`, qui simplifie le processus d'analyse.

```swift
import Foundation

let dateString = "2023-04-30T15:00:00+00:00"
let isoFormatter = ISO8601DateFormatter()
if let date = isoFormatter.date(from: dateString) {
    print("Date ISO8601 analysée : \(date)")
} else {
    print("Échec de l'analyse de la date ISO8601")
}
// Exemple de sortie : Date ISO8601 analysée : 2023-04-30 15:00:00 +0000
```

### Utiliser une bibliothèque tierce : SwiftDate
Bien que Swift fournisse des outils robustes pour l'analyse de dates, les bibliothèques tierces comme SwiftDate offrent encore plus de flexibilité et de commodité. Après avoir ajouté SwiftDate à votre projet, l'analyse devient aussi simple que :

```swift
import SwiftDate

let dateString = "April 30, 2023"
if let date = dateString.toDate("MMMM dd, yyyy") {
    print("Date analysée avec SwiftDate : \(date)")
} else {
    print("Échec de l'analyse de la date avec SwiftDate")
}
// Exemple de sortie : Date analysée avec SwiftDate : 2023-04-30 00:00:00 +0000
```

SwiftDate simplifie l'analyse avec un langage naturel et une large gamme de formats de dates, en faisant un ajout puissant à votre boîte à outils de programmation Swift.
