---
title:                "Swift: Calcul d'une date dans le futur ou le passé"
simple_title:         "Calcul d'une date dans le futur ou le passé"
programming_language: "Swift"
category:             "Swift"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/swift/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Pourquoi

Calculer une date dans le futur ou le passé peut être utile dans de nombreuses situations, que ce soit pour des rappels, des réservations ou simplement pour savoir quelle journée tombe un événement important.

## Comment Faire

Il existe plusieurs façons de calculer une date dans le futur ou le passé en utilisant le langage Swift. La première méthode consiste à utiliser la fonction `Date()` qui renvoie la date actuelle, puis à utiliser la méthode `addingTimeInterval()` pour ajouter ou soustraire un certain nombre de secondes, minutes, heures, jours, mois ou années. Voici un exemple de code pour calculer une date dans 7 jours :
```Swift
let today = Date()
let futureDate = today.addingTimeInterval(7 * 24 * 3600)
print(futureDate)

// Output: 2021-07-16 14:31:22 +0000
```

Une autre façon de calculer une date dans le futur ou le passé est d'utiliser le type `DateComponent` et la classe `Calendar`. Cette méthode est plus précise car elle prend en compte les différents calendriers (grégorien, hébraïque, islamique, etc.) et les fuseaux horaires. Voici un exemple de code pour calculer une date dans 3 mois :
```Swift
let currentDate = Date()
var dateComponents = DateComponents()
dateComponents.month = 3
let futureDate = Calendar.current.date(byAdding: dateComponents, to: currentDate)

// Output: Optional(2021-10-09 14:31:22 +0000)
```

## Plongée en Profondeur

Il est important de comprendre que le langage Swift utilise le temps Unix pour stocker les dates et les heures, qui est la mesure du temps écoulé en secondes depuis le 1er janvier 1970 à minuit UTC. Cela signifie que les calculs de dates dans le futur ou le passé se basent sur cette mesure, il est donc important de prendre cela en compte lors de la conversion de temps dans différentes unités.

Un autre point important à noter est que les calculs de date dans le futur ou le passé peuvent être affectés par certains facteurs externes tels que les années bissextiles, les changements d'heure et les changements de fuseau horaire. Il est donc recommandé d'utiliser la méthode de calcul de date la plus précise en fonction de vos besoins.

## Voir Aussi

- Apple Developer Documentation: [Manipulating Dates and Times Using the Calendar](https://developer.apple.com/documentation/foundation/calendar)
- Hacking With Swift: [Working with Dates and Times in Swift](https://www.hackingwithswift.com/articles/142/working-with-dates-and-times-in-swift)