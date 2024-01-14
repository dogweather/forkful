---
title:    "Swift: Calculer une date dans le futur ou le passé"
keywords: ["Swift"]
---

{{< edit_this_page >}}

## Pourquoi

Si vous êtes un programmeur Swift, vous savez peut-être déjà que la manipulation de dates est un élément essentiel de la programmation dans n'importe quel langage. Que vous ayez besoin de calculer une date dans le futur pour une tâche planifiée ou une date dans le passé pour un historique, savoir comment manipuler les dates est une compétence précieuse. Dans cet article, nous allons vous montrer comment calculer une date dans le futur ou dans le passé en utilisant Swift.

## Comment faire

Tout d'abord, vous devez importer le framework Foundation car c'est là que se trouvent les classes de gestion de dates en Swift. Ensuite, vous pouvez créer une instance de la classe NSDate représentant la date actuelle.

```Swift
import Foundation

let now = NSDate()
```

Pour calculer une date dans le futur, nous allons utiliser la méthode `dateByAddingTimeInterval` de l'objet `now`. Cette méthode prend un intervalle de temps en secondes et renvoie une nouvelle date en ajoutant cet intervalle à la date d'origine.

```Swift
let futureDate = now.dateByAddingTimeInterval(3600) // 3600 secondes = 1 heure
```

De même, pour calculer une date dans le passé, nous pouvons utiliser la méthode `dateByAddingTimeInterval` en utilisant un intervalle de temps négatif.

```Swift
let pastDate = now.dateByAddingTimeInterval(-86400) // -86400 secondes = 1 jour
```

Pour formater la date de manière lisible pour l'utilisateur, nous pouvons utiliser un objet `NSDateFormatter`.

```Swift
let dateFormatter = NSDateFormatter()
dateFormatter.dateStyle = .mediumStyle
dateFormatter.timeStyle = .shortStyle

print(dateFormatter.stringFromDate(futureDate)) //affiche une date au format moyen et l'heure au format court
```

## Une plongée en profondeur

Maintenant que nous savons comment calculer une date dans le futur ou dans le passé, il est important de comprendre comment les dates sont représentées en Swift. Les dates sont mesurées en secondes depuis une date de référence, le 1er janvier 2001 à minuit UTC. Cela signifie que si vous ajoutez ou soustrayez un intervalle de temps à une date, l'heure peut changer en fonction du fuseau horaire de l'utilisateur.

De plus, la classe NSDate n'est pas mutable, ce qui signifie que vous ne pouvez pas changer la valeur d'un objet NSDate. Si vous avez besoin de modifier une date, vous devez créer une nouvelle instance de date avec la valeur modifiée.

## Voir aussi

- [Documentation Apple sur la classe NSDate](https://developer.apple.com/documentation/foundation/nsdate)
- [Guide de programmation Swift pour les dates et les heures](https://docs.swift.org/swift-book/LanguageGuide/BasicOperators.html)
- [Article sur la manipulation des dates en Swift](https://medium.com/@shubham4rql/swift-tutorial-adding-dates-and-constants-1074c0a6c89b)