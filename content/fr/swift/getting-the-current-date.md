---
title:                "Obtenir la date actuelle"
html_title:           "Swift: Obtenir la date actuelle"
simple_title:         "Obtenir la date actuelle"
programming_language: "Swift"
category:             "Swift"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/swift/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Pourquoi

Vous êtes-vous déjà demandé comment obtenir la date actuelle dans vos programmes Swift ? Peut-être que vous avez besoin d'afficher la date dans votre application ou peut-être que vous voulez simplement l'utiliser pour des opérations de calcul. Dans cet article, je vais vous montrer comment obtenir la date actuelle en utilisant Swift.

## Comment faire

Pour obtenir la date actuelle en Swift, nous allons utiliser la classe `Date` fournie par le framework `Foundation`. Suivez les étapes ci-dessous pour voir comment l'utiliser dans votre code.

```Swift
// Importation du framework Foundation
import Foundation

// Obtenir la date actuelle
let currentDate = Date()

// Convertir la date en chaîne de caractères
let dateString = "\(currentDate)"

// Imprimer la date au format par défaut
print(dateString)

// Imprimer la date en utilisant un format spécifique
let dateFormatter = DateFormatter()
dateFormatter.dateFormat = "dd/MM/YYYY"
print(dateFormatter.string(from: currentDate))
```

L'exemple ci-dessus montre comment obtenir la date actuelle en utilisant la classe `Date`. Nous pouvons utiliser la méthode `print()` pour afficher la date au format par défaut ou utiliser un objet `DateFormatter` pour choisir un format spécifique.

## Plongée en profondeur

La classe `Date` contient également d'autres méthodes utiles pour manipuler les dates, telles que `addingTimeInterval` pour ajouter un certain nombre de secondes à une date donnée et `compare` pour comparer deux dates. Vous pouvez également utiliser la classe `Calendar` pour travailler avec des composants tels que l'heure, le jour et le mois d'une date.

Pour en savoir plus sur la manipulation des dates en Swift, je vous recommande de consulter la documentation officielle de Swift sur les classes `Date` et `Calendar`.

## Voir aussi

- [Documentation officielle de Swift sur les dates](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html#ID293)
- [Documentation officielle de Swift sur les calendriers](https://developer.apple.com/documentation/foundation/calendar)