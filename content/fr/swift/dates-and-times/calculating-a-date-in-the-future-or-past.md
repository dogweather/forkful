---
date: 2024-01-20 17:32:03.595670-07:00
description: 'How to: Swift offre `DateComponents` et `Calendar` pour manipuler les
  dates. Voici un exemple simple.'
lastmod: '2024-03-13T22:44:58.238865-06:00'
model: gpt-4-1106-preview
summary: Swift offre `DateComponents` et `Calendar` pour manipuler les dates.
title: "Calcul d'une date future ou pass\xE9e"
weight: 26
---

## How to:
Swift offre `DateComponents` et `Calendar` pour manipuler les dates. Voici un exemple simple:

```Swift
import Foundation

// Aujourd'hui
let today = Date()

// Le calendrier actuel
let calendar = Calendar.current

// Ajouter 5 jours
if let fiveDaysLater = calendar.date(byAdding: .day, value: 5, to: today) {
    print("Dans 5 jours : \(fiveDaysLater)")
}

// Soustraire 30 minutes
if let thirtyMinutesBefore = calendar.date(byAdding: .minute, value: -30, to: today) {
    print("Il y a 30 minutes : \(thirtyMinutesBefore)")
}
```

Sample output:
```
Dans 5 jours : 2023-04-15 14:23:44 +0000
Il y a 30 minutes : 2023-04-10 13:53:44 +0000
```

## Deep Dive
Historiquement, gérer le temps en programmation a toujours été délicat, surtout avec les fuseaux horaires et les changements d'heure. Avant `DateComponents`, on manipulait les secondes directement, risqué et imprécis. Swift simplifie avec `Calendar`, permettant des calculs clairs et précis.

Alternatives? On pourrait utiliser des bibliothèques tierces comme `SwiftDate`, mais `Foundation` de Swift est souvent suffisant et bien intégré.

Détails d'implémentation: `Calendar` donne le contexte temporel (fuseaux, changements d'heure) et `DateComponents` les éléments à ajouter ou soustraire (jours, minutes). Le système gère le reste, y compris les irrégularités comme les années bissextiles.

## See Also
- Documentation Apple pour `DateComponents`: [Documentation DateComponents](https://developer.apple.com/documentation/foundation/datecomponents)
- Documentation Apple pour `Calendar`: [Documentation Calendar](https://developer.apple.com/documentation/foundation/calendar)
- SwiftDate, une puissante bibliothèque pour manipuler les dates: [SwiftDate on GitHub](https://github.com/malcommac/SwiftDate)
