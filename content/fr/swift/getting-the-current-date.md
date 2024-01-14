---
title:    "Swift: Obtenir la date actuelle."
keywords: ["Swift"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/swift/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Pourquoi

Êtes-vous intéressés à apprendre à obtenir la date actuelle en programmation Swift? Que vous soyez un développeur débutant ou expérimenté, connaître les différentes méthodes pour obtenir la date actuelle peut être très utile dans vos projets. Cela peut vous aider à afficher l'heure exacte de vos messages, enregistrer des horodatages dans vos bases de données et bien plus encore.

## Comment faire

```Swift
// L'utilisation de la classe Date pour obtenir la date actuelle.
let date = Date()

// Utiliser un DateFormatter pour formater la date selon vos préférences.
let dateFormatter = DateFormatter()
dateFormatter.dateFormat = "dd.MM.yyyy HH:mm:ss"
let dateString = dateFormatter.string(from: date)

// Afficher le résultat.
print(dateString) // output: 07.08.2021 18:30:15
```

Vous pouvez également utiliser des fonctions intégrées pour obtenir la date actuelle. Par exemple, la fonction `Date()`, qui renvoie la date et l'heure actuelles, ou la fonction `timeIntervalSince1970`, qui renvoie le nombre de secondes écoulées depuis le 1er janvier 1970.

```Swift
// Utiliser la fonction Date().
let now = Date()

// Récupérer le nombre de secondes écoulées depuis 1970.
let timeInterval = now.timeIntervalSince1970

// Afficher le résultat.
print("Il s'est écoulé \(timeInterval) secondes depuis le 1er janvier 1970.")
// output: Il s'est écoulé 1628397119.0 secondes depuis le 1er janvier 1970.
```

## Plongée en profondeur

Maintenant que vous savez comment obtenir la date actuelle en Swift, jetons un œil à certaines des options de formatage possibles avec `DateFormatter`.

| Symbole | Signification |
|-----|----|
| `dd` | Jour du mois (01-31) |
| `MM` | Mois (01-12) |
| `yyyy` | Année (4 chiffres) |
| `HH` | Heure au format 24 heures (00-23) |
| `mm` | Minute (00-59) |
| `ss` | Seconde (00-59) |

Il existe de nombreux autres symboles de formatage disponibles, en fonction de vos besoins. Vous pouvez également utiliser des options de localisation pour afficher la date dans un format spécifique à un pays ou à une langue.

## Voir aussi

- [Documentation officielle de la classe Date](https://developer.apple.com/documentation/foundation/date)
- [Documentation officielle de DateFormatter](https://developer.apple.com/documentation/foundation/dateformatter)
- [Guide de localisation de la date et de l'heure en Swift](https://nshipster.com/dateformatter/)