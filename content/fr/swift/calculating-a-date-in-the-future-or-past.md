---
title:                "Calculer une date dans le futur ou le passé"
html_title:           "Swift: Calculer une date dans le futur ou le passé"
simple_title:         "Calculer une date dans le futur ou le passé"
programming_language: "Swift"
category:             "Swift"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/swift/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Quoi et Pourquoi ?

La calcul d'une date future ou passée en programmation consiste à ajouter ou soustraire un certain laps de temps à une date donnée. C’est une technique couramment utilisée par les développeurs pour gérer l'échéance, l'organisation des événements et les rappels.

## Comment faire :

En Swift, vous pouvez utiliser la classe `DateComponents` pour calculer une date future ou passée. Voici comment faire :

```Swift
import Foundation

let aujourdHui = Date()
var composantsDeDate = DateComponents()

composantsDeDate.day = 10

if let dixJoursPlusTard = Calendar.current.date(byAdding: composantsDeDate, to: aujourdHui) {
    print(dixJoursPlusTard)
}
```
Ce qui afficherait une date 10 jours à partir d'aujourd'hui.

```Swift
composantsDeDate.day = -10

if let dixJoursAvant = Calendar.current.date(byAdding: composantsDeDate, to: aujourdHui) {
    print(dixJoursAvant)
}
```
Cette fois, le code affiche une date 10 jours avant la date actuelle.

## Plongeons plus profondément

Historiquement, la gestion des dates a toujours été un défi en programmation, surtout si l'on prend en compte les différentes zones horaires, les systèmes de calendrier et le passage à l'heure d'été. Swift résout ce problème en fournissant une collection complète de classes et fonctions de gestion du temps.

Autrement, vous pourriez manipuler manuellement les millisecondes d'une date, mais cela pourrait être source d'erreurs et difficile à maîtriser.

Lors de la mise en œuvre des calculs de date, veillez à prendre en compte les variations annuelles comme les années bissextiles et le changement d'heure.

## Voir également

Pour approfondir vos connaissances sur le sujet, voici quelques liens utiles :

1. Documentation officielle de Swift sur `DateComponents` : "https://developer.apple.com/documentation/foundation/datecomponents"
2. Un guide sur le calcul des dates en Swift : "https://www.hackingwithswift.com/quick-start/understanding-swift/how-to-calculate-the-difference-between-two-dates"
3. Un autre article sur la manipulation des dates et heures en Swift : "https://www.raywenderlich.com/5823-date-and-time-programming-guide"