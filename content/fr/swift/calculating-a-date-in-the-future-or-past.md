---
title:                "Swift: Calculer une date dans le futur ou le passé."
programming_language: "Swift"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/swift/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Pourquoi

Calculer une date à l'avenir ou dans le passé peut être utile pour diverses raisons, que ce soit pour planifier des événements, vérifier des délais ou simplement pour satisfaire notre curiosité.

## Comment Faire

Pour calculer une date dans le futur ou dans le passé en Swift, nous allons utiliser la classe `Calendar` et sa méthode `date(byAdding:to:)`. Cette méthode prend en paramètres un entier représentant l'unité de temps à ajouter ou à soustraire (jours, mois, années, etc.) et un objet de type `Date` représentant la date de départ. Voici un exemple de code pour calculer la date d'aujourd'hui dans un an :

```Swift
let calendar = Calendar.current
let today = Date()
let nextYear = calendar.date(byAdding: .year, value: 1, to: today)
```

La méthode `date(byAdding:to:)` renvoie un objet de type `Date?`, donc il est important de vérifier s'il y a une valeur avant de l'utiliser.

## Plongée en Profondeur

Il est également possible de calculer des dates en utilisant des intervalles de temps plus précis tels que les jours de semaine ou les heures. Pour cela, nous allons utiliser la méthode `dateComponents(_:from:)` de la classe `Calendar`. Cette méthode prend en paramètres un ensemble de composants de temps (jour, mois, heure, etc.) et un objet de type `Date` qui représente la date de départ. Elle renvoie un objet de type `DateComponents?` qui contient les composants de temps calculés.

Voici un exemple de code pour calculer la date du prochain vendredi à 19h :

```Swift
let calendar = Calendar.current
let today = Date()
let nextFriday = calendar.date(byAdding: .day, value: 5, to: today) // Ajoute 5 jours pour obtenir le prochain vendredi
let nextFridayAt7PM = calendar.dateComponents([.year, .month, .day, .hour], from: nextFriday!)
nextFridayAt7PM.hour = 19 // Définit l'heure à 19h
let nextFridayDate = calendar.date(from: nextFridayAt7PM)
```

Il est important de noter que la classe `Calendar` utilise par défaut le calendrier du système de l'utilisateur, il est donc important de spécifier le calendrier approprié si vous travaillez avec un calendrier différent.

## Voir Aussi

- [Documentation officielle de la classe `Calendar`](https://developer.apple.com/documentation/foundation/calendar)
- [Documentation officielle de la méthode `date(byAdding:to:)`](https://developer.apple.com/documentation/foundation/calendar/2293435-date)
- [Documentation officielle de la méthode `dateComponents(_:from:)`](https://developer.apple.com/documentation/foundation/calendar/2293084-datecomponents)