---
title:                "Comparaison de deux dates"
html_title:           "Swift: Comparaison de deux dates"
simple_title:         "Comparaison de deux dates"
programming_language: "Swift"
category:             "Swift"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/swift/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi?
Comparer deux dates est une tâche courante dans la programmation Swift. Cela implique de vérifier si une date est antérieure, égale ou postérieure à une autre, en utilisant différents critères tels que l'heure, la minute, le jour, etc. Les programmeurs le font pour une variété de raisons, notamment pour trier des données chronologiquement, pour calculer des écarts de temps ou pour vérifier l'état d'expiration d'un élément.

## Comment faire:
La comparaison de deux dates peut être effectuée de différentes manières en Swift, en fonction de vos besoins spécifiques. Voici quelques exemples de code et leur sortie correspondante:

```Swift
let date1 = Date() //Date actuelle
let date2 = Date(timeIntervalSinceNow: 86400) //Date dans 24 heures

date1 == date2 //false
date1 < date2 //true
date1 > date2 //false

let diffComponents = Calendar.current.dateComponents([.hour, .minute], from: date2, to: date1)
print(diffComponents) //hour: Optional(-24) | minute: Optional(0)
```

Dans cet exemple, nous créons deux dates, l'une étant l'instant présent et l'autre étant dans 24 heures. Nous comparons ensuite ces deux dates et calculons également la différence de temps en heures et en minutes.

## Deep Dive:
La notion de temps et de dates a toujours été un défi pour les programmeurs, car elle peut varier en fonction de l'emplacement géographique et des fuseaux horaires. Swift utilise le calendrier gorgien qui a été introduit en 1582 pour calculer les écarts de temps. Il existe également des alternatives à la méthode de comparaison de dates en utilisant des bibliothèques tierces telles que NSDate et Moment.js.

Il est important de noter que la comparaison de dates ne tient pas compte de la localisation et de la mise en forme. Il est donc préférable de convertir les dates en un format approprié avant de les comparer.

## Voir aussi:
Pour en savoir plus sur la gestion du temps et des dates en Swift, consultez la documentation officielle: https://developer.apple.com/documentation/foundation/date et https://developer.apple.com/documentation/foundation/calendar

Pour utiliser des fonctions plus poussées de comparaison de dates, vous pouvez également explorer la bibliothèque open source NSDate: https://github.com/BryanHaley/NSDate-String-Extensions

Merci d'avoir lu cet article sur la comparaison de dates en Swift. Nous espérons qu'il vous a été utile dans votre compréhension de cette tâche importante en programmation. Bonne codification!