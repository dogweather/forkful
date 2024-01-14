---
title:                "Swift: Comparer deux dates"
simple_title:         "Comparer deux dates"
programming_language: "Swift"
category:             "Swift"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/swift/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Pourquoi

Vous êtes peut-être en train de vous demander pourquoi il pourrait être utile de comparer deux dates en programmation Swift. En fait, comparer des dates est souvent une tâche courante en développement d’applications, que ce soit pour vérifier si une date est antérieure ou postérieure à une autre, ou pour calculer le temps écoulé entre deux dates.

## Comment faire

Poursuivez votre lecture pour découvrir comment comparer deux dates en Swift grâce à des exemples de code et des résultats affichés pour chaque étape. Vous pourrez ainsi appliquer ces connaissances à vos propres projets et utiliser cette fonctionnalité pratique de Swift.

```Swift
// Création de deux objets Date
let date1 = Date()
let date2 = Date()

// Utilisation de l'opérateur">"
if date1 > date2 {
    print("date1 est après date2")
}

// Utilisation de l'opérateur "<"
if date1 < date2 {
    print("date1 est avant date2")
}

// Utilisation de la fonction "compare"
let comparaison = date1.compare(date2)
switch comparaison {
case .orderedAscending:
    print("date1 est avant date2")
case .orderedSame:
    print("les deux dates sont identiques")
case .orderedDescending:
    print("date1 est après date2")
}
```

En utilisant l'opérateur ">", nous comparons si la date1 est postérieure à la date2. De la même manière, nous pouvons utiliser l'opérateur "<" pour vérifier si une date est antérieure à une autre. La fonction "compare" retourne un enum qui nous permet d'effectuer des comparaisons plus complexes en utilisant une instruction switch.

## Deep Dive

Pour aller plus loin, il est intéressant de noter qu'il existe différentes manières de comparer des dates en Swift selon nos besoins. Par exemple, si nous voulons vérifier si deux dates ont la même valeur, nous pouvons utiliser la méthode "isEqual", qui renvoie un booléen.

```Swift
if date1.isEqual(date2) {
    print("les deux dates sont identiques")
}
```

L'utilisation de la fonction "Calendar" peut également être utile pour effectuer des comparaisons en tenant compte des fuseaux horaires ou d'autres paramètres de la date.

## Voir aussi
Consultez ces liens pour en savoir plus sur la comparaison de dates en Swift :

- [Comparing dates in Swift](https://fluffy.es/comparing-dates-swift/)
- [How to compare dates in Swift](https://www.hackingwithswift.com/example-code/language/how-to-compare-dates-in-swift)
- [Working with dates in Swift](https://medium.com/flawless-app-stories/working-with-dates-in-swift-4899b2422e33)

Maintenant que vous avez toutes les informations sur la comparaison de dates en Swift, vous pouvez l'appliquer à vos propres projets et gagner du temps et de la précision dans votre développement d'applications. Bonne programmation !