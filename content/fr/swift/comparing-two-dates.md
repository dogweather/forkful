---
title:                "Swift: Comparaison de deux dates"
programming_language: "Swift"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/swift/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Pourquoi

Dans le monde de la programmation Swift, il est souvent nécessaire de comparer deux dates. Que ce soit pour des fonctionnalités liées à la gestion des rendez-vous, des tâches à effectuer ou tout simplement pour vérifier si une date est passée ou future, la comparaison de dates est une compétence essentielle pour tout développeur Swift.

## Comment faire

La première étape pour comparer deux dates en Swift est de les convertir en objets de type `Date`. Ensuite, il existe plusieurs méthodes pour effectuer la comparaison, en fonction de nos besoins.

```Swift
let dateFormatter = DateFormatter() // Création d'un objet DateFormatter
dateFormatter.dateFormat = "dd/MM/yyyy" // Format souhaité pour notre date

let date1 = dateFormatter.date(from: "10/02/2020") // Conversion de notre première date en objet de type Date
let date2 = dateFormatter.date(from: "15/02/2020") // Conversion de notre deuxième date en objet de type Date

// Comparaison de dates en utilisant l'opérateur ">"
if date1 > date2 {
    print("La date 1 est plus récente que la date 2")
} else {
    print("La date 2 est plus récente que la date 1")
}

// Comparaison de dates en utilisant la méthode "compare"
let result = date1.compare(date2)
if result == .orderedDescending {
    print("La date 1 est plus récente que la date 2")
} else if result == .orderedAscending {
    print("La date 2 est plus récente que la date 1")
} else {
    print("Les deux dates sont identiques")
}
```

### Output :

```
La date 2 est plus récente que la date 1
La date 2 est plus récente que la date 1
```

Bien sûr, il est également possible de comparer les dates en prenant en compte les heures, minutes et secondes en utilisant `Calendar` et `DateComponents`.

## Plongée en profondeur

Il est important de comprendre que la comparaison de dates en Swift peut être influencée par la timezone du dispositif sur lequel l'application est utilisée. Il est donc recommandé de définir explicitement la timezone lors de la création des objets de type `Date`.

Une autre astuce utile est d'utiliser `Calendar.current` pour obtenir les informations sur la timezone actuelle du dispositif, plutôt que d'essayer de déterminer la timezone manuellement.

## Voir aussi

- [Documentation officielle Apple - Comparer des dates](https://developer.apple.com/documentation/foundation/date)
- [Tutoriel sur la comparaison de dates en Swift](https://learnappmaking.com/compare-dates-swift-how-to/)