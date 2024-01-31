---
title:                "Analyse d'une date à partir d'une chaîne de caractères"
date:                  2024-01-20T15:38:25.777584-07:00
simple_title:         "Analyse d'une date à partir d'une chaîne de caractères"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/swift/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## What & Why?
En Swift, le "parsing" d'une date à partir d'une chaîne de caractères convertit le texte en un type de données `Date`. Pourquoi? Par exemple, pour enregistrer des horodatages de serveur ou des entrées utilisateur dans une forme manipulable.

## How to:
```Swift
import Foundation

let dateString = "2023-04-01T12:45:00+0000"
let dateFormatter = DateFormatter()
dateFormatter.dateFormat = "yyyy-MM-dd'T'HH:mm:ssZ"
if let parsedDate = dateFormatter.date(from: dateString) {
    print("La date analysée est: \(parsedDate)")
} else {
    print("Erreur de parsing.")
}
```
Output:
```
La date analysée est: 2023-04-01 12:45:00 +0000
```

## Deep Dive
Historiquement, le "parsing" de dates en Swift dépendait de `NSDateFormatter`, partie de Cocoa. Dans Swift, on l'a renommé en `DateFormatter`. On a des options: utiliser `ISO8601DateFormatter` pour le format ISO 8601, ou créer des formats personnalisés avec `DateFormatter`.

Le choix du format de date est critique. Si votre chaîne ne correspond pas exactement au format spécifié, le parsing échouera. De plus, attention aux décalages horaires et aux configurations régionales lorsque vous interagissez avec des utilisateurs internationaux.

Enfin, bien qu'on utilise souvent `DateFormatter` pour sa facilité, considérez `DateComponents` pour déconstruire ou comparer des dates, et `Calendar` pour des calculs de date.

## See Also
- Documentation officielle `DateFormatter`: https://developer.apple.com/documentation/foundation/dateformatter
- Guide des formats de date Unicode: http://www.unicode.org/reports/tr35/tr35-31/tr35-dates.html#Date_Format_Patterns
- Stack Overflow pour des questions/réponses spécifiques: https://stackoverflow.com/questions/tagged/swift+dateformatter
