---
title:                "Analyser une date à partir d'une chaîne"
html_title:           "Clojure: Analyser une date à partir d'une chaîne"
simple_title:         "Analyser une date à partir d'une chaîne"
programming_language: "Swift"
category:             "Swift"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/swift/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

# Analyser une date à partir d'une chaîne en Swift

## Qu'est-ce et Pourquoi?
Analyser une date à partir d'une chaîne c'est transformer un texte qui représente une date en un objet `Date` que Swift peut comprendre. Les programmeurs le font pour manipuler et utiliser les dates dans leur code.

## Comment faire:
Voici le code:

```Swift
let date = Date()
let formatter = DateFormatter()
formatter.dateFormat = "dd.MM.yyyy"
let formattedDate = formatter.string(from: date)
```

Cela donnera une date formatée sous la forme: "jour.mois.année". Par exemple : "01.01.2021".

Et si vous avez une chaîne et que vous voulez la convertir en Date:

```Swift
let dateString = "01-01-2021"
let dateFormatter = DateFormatter()
dateFormatter.dateFormat = "dd-MM-yyyy"
if let date = dateFormatter.date(from: dateString) {
    print(date)
}
```

Cela va afficher "2021-01-01 00:00:00 +0000" dans la console.

## Plongée en profondeur:
Historiquement, différentes régions et langues ont des conventions différentes pour formater les dates. Swift, avec ses formateurs de date, offre une grande flexibilité en prenant en charge de nombreux formats de date et permet une localisation facile.

Comme alternatives, pour les cas simples, on peut également utiliser `ISO8601DateFormatter` ou convertir directement la chaîne en `Date` en utilisant `.iso8601`. Cependant, `DateFormatter` offre plus de flexibilité et de contrôle.

Quand vous utilisez `date(from:)` fonction, il renvoie une valeur facultative (`Date?`), car il se peut qu'il ne puisse pas analyser la chaîne si le format en chaîne ne correspond pas au format de date que vous avez spécifié.

## Voir aussi:
- Documentation officielle Apple sur DateFormatter [ici](https://developer.apple.com/documentation/foundation/dateformatter)
- Guide complet sur la manipulation des dates et des heures dans Swift [ici](https://www.hackingwithswift.com/articles/141/8-powerful-swift-features-that-few-people-know-about)
- Pour comprendre les différents formats de date: [ici](https://nsdateformatter.com/)