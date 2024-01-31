---
title:                "Obtenir la date actuelle"
date:                  2024-01-20T15:16:41.974622-07:00
simple_title:         "Obtenir la date actuelle"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/swift/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Quoi et Pourquoi ?
Obtenir la date actuelle en Swift, c'est capturer le moment présent dans notre code. Les développeurs s'en servent pour tout, des logs à la gestion des évènements temps-réel.

## Comment faire :
```Swift
import Foundation

// Obtenir la date et l'heure actuelle
let now = Date()

print(now)
```

Exemple de sortie :
```
2023-04-05 12:24:06 +0000
```

## Plongée Profonde
Historiquement, Swift a hérité de la gestion du temps de Objective-C et de ses classes fondationnelles comme `NSDate`. Mais avec `Date` en Swift, tout est plus swifty – plus simple à écrire et à lire. Pour les alternatives, on pourrait se tourner vers des bibliothèques tierces comme `SwiftDate`, mais pour une utilisation de base, `Date` suffit et est ultra performant. Le détail qui compte : `Date()` en Swift renvoie le temps universel coordonné (UTC), donc pensez à gérer les fuseaux horaires pour une utilisation locale.

## Voir Aussi
- La documentation Apple sur la classe Date : [Date - Foundation | Apple Developer Documentation](https://developer.apple.com/documentation/foundation/date)
- Pour des besoins avancés, SwiftDate pourrait être intéressant : [SwiftDate Documentation](https://malcommac.github.io/SwiftDate/)
