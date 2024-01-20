---
title:                "Obtenir la date actuelle"
html_title:           "Bash: Obtenir la date actuelle"
simple_title:         "Obtenir la date actuelle"
programming_language: "Swift"
category:             "Swift"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/swift/getting-the-current-date.md"
---

{{< edit_this_page >}}

# Swift: Comment obtenir la date actuelle

## Quoi & Pourquoi?

Obtenir la date actuelle est une manière pour les programmeurs de marquer des événements ou des actions dans leurs scripts. Cela peut être utilisé pour le suivi des erreurs, les horodatages et la quantification du temps écoulé.

## Comment faire:

La méthode standard d'obtention de la date courante en Swift implique l'utilisation de la classe `Date`. Regardez cet exemple:

```Swift
let maintenant = Date()
print("Date et heure actuelles: \(maintenant)")
```

Et l'affichage serait:

```Swift
Date et heure actuelles: 2022-01-15 15:37:30 +0000
```

## Plongée en profondeur

Historiquement, Swift dérive la classe `Date` de sa bibliothèque de fondation. Autrefois, nous utilisions `NSDate` en Objective-C.

Des alternatives existent, comme l'utilisation de `DateFormatter` pour personnaliser l'affichage de la date. Exemple:

```Swift
let maintenant = Date()

let formatteur = DateFormatter()
formatteur.dateStyle = .long

let dateString = formatteur.string(from: maintenant)
print("Date formatée: \(dateString)")
```

Cela afficherait:

```Swift
Date formatée: 15 janvier 2022
```

La classe `Date` enregistre le temps écoulé depuis une date de référence (1er janvier 2001, 00:00:00 GMT). Ainsi, lorsque vous créez une nouvelle instance de `Date`, vous obtenez l'intervalle de temps entre maintenant et cette date de référence.

## Voir Aussi

- Documentation Apple sur [Date](https://developer.apple.com/documentation/foundation/date)
- How to format dates with [Swift DateFormatter](https://www.hackingwithswift.com/example-code/system/how-to-format-dates-with-dateformatter)
- Swift by Sundell's article on [Dealing with dates](https://www.swiftbysundell.com/basics/dates/)