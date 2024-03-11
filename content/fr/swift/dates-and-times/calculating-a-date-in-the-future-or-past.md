---
date: 2024-01-20 17:32:03.595670-07:00
description: "Calculer une date dans le futur ou le pass\xE9, c'est juste ajouter\
  \ ou soustraire du temps \xE0 une date donn\xE9e. Les programmeurs font \xE7a pour\
  \ g\xE9rer des\u2026"
lastmod: '2024-03-11T00:14:32.118084-06:00'
model: gpt-4-1106-preview
summary: "Calculer une date dans le futur ou le pass\xE9, c'est juste ajouter ou soustraire\
  \ du temps \xE0 une date donn\xE9e. Les programmeurs font \xE7a pour g\xE9rer des\u2026"
title: "Calcul d'une date future ou pass\xE9e"
---

{{< edit_this_page >}}

## What & Why?
Calculer une date dans le futur ou le passé, c'est juste ajouter ou soustraire du temps à une date donnée. Les programmeurs font ça pour gérer des rappels, des échéances ou des intervalles temporels spécifiques.

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
