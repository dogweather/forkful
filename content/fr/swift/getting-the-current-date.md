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

## Qu'est-ce que c'est et pourquoi?
Récupérer la date actuelle est une tâche courante en programmation, car cela permet de suivre le temps et la durée des opérations. Cela peut également être utile pour afficher la date dans une application ou pour effectuer des calculs basés sur la date actuelle.

## Comment faire:
Voici comment récupérer la date actuelle en utilisant Swift:

```Swift
let currentDate = Date() // En utilisant l'instance Date()
print(currentDate) // Affiche la date et l'heure actuelles
```

Vous pouvez également personnaliser le format de date en utilisant un `DateFormatter`:

```Swift
let dateFormatter = DateFormatter()
dateFormatter.dateFormat = "dd/MM/yyyy"
let currentDate = Date()
let dateString = dateFormatter.string(from: currentDate)
print(dateString) // Affiche la date actuelle au format jour/mois/année
```

## Plongée en profondeur:
Avant la sortie de Swift, les développeurs utilisaient principalement `NSDate` pour récupérer la date actuelle, mais cela a été remplacé par `Date` dans Swift. Dans certaines situations, vous pouvez également obtenir la date et l'heure du Fuseau horaire actuel en utilisant `TimeZone.current` en combinaison avec `Date()`, plutôt que d'utiliser directement `Date()`.

## Voir aussi:
Pour plus d'informations sur la récupération de la date actuelle en Swift, vous pouvez consulter la documentation officielle de Swift et la communauté des développeurs, ainsi que des tutoriels en ligne pour des exemples plus approfondis.