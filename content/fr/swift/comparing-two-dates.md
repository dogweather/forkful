---
title:                "Comparer deux dates"
html_title:           "Swift: Comparer deux dates"
simple_title:         "Comparer deux dates"
programming_language: "Swift"
category:             "Swift"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/swift/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Pourquoi

Si vous travaillez avec des données temporelles dans votre code Swift, il est probable que vous ayez besoin de comparer deux dates à un moment donné. Cela peut être utile lors de la planification d'événements, de la mise en œuvre de logiques de validation, ou simplement pour faciliter la compréhension des données chronologiques dans votre application. Dans cet article, nous allons explorer comment comparer deux dates en utilisant Swift.

## Comment faire

Pour comparer deux dates en Swift, nous allons utiliser la méthode `compare()` de la classe `Date`.

```
let date1 = Date()
let date2 = Date(timeIntervalSinceNow: 3600)

if date1.compare(date2) == .orderedAscending {
    print("date1 est avant date2")
} else if date1.compare(date2) == .orderedDescending {
    print("date1 est après date2")
} else {
    print("date1 est égal à date2")
}
```

Dans cet exemple, nous créons deux objets `Date`, `date1` et `date2`, avec une différence d'une heure entre eux. En utilisant la méthode `compare()`, nous pouvons comparer ces deux dates et obtenir un résultat indiquant si `date1` est avant, après ou égal à `date2`. Dans ce cas, le résultat serait "date1 est avant date2".

Nous pouvons également utiliser la méthode `==` pour comparer directement deux dates.

```
if date1 == date2 {
    print("Les deux dates sont égales")
}
```

En plus de ces comparaisons simples, nous pouvons également utiliser des opérateurs de comparaison (`<`, `>`, `<=`, `>=`) pour évaluer si une date est avant ou après une autre. Par exemple :

```
if date1 < date2 {
    print("date1 est avant date2")
}
```

## Plongée en profondeur

Lorsque nous comparons deux dates en Swift, il est important de comprendre comment les dates sont stockées et comparées dans le langage. En interne, les dates sont représentées comme une valeur à virgule flottante, mesurant le nombre de secondes écoulées depuis le 1er janvier 2001 à 00h00 UTC. Cela signifie que les dates sont comparées en fonction du temps universel plutôt que de la date et de l'heure locales.

De plus, lors de la comparaison de deux dates avec une précision plus fine que les secondes, la méthode `compare()` utilise la propriété `timeIntervalSince1970` pour déterminer l'ordre. Ceci est important à prendre en compte lors de la comparaison de dates avec une précision plus fine, comme les millisecondes ou les nanosecondes.

## Voir aussi

Si vous souhaitez en savoir plus sur la manipulation de dates en Swift, vous pouvez consulter les liens suivants :

- [Documentation officielle sur la classe `Date`](https://developer.apple.com/documentation/foundation/date)
- [Tutoriel sur les dates en Swift](https://www.raywenderlich.com/7799031-swift-date-how-to-work-with-dates-in-swift)

Maintenant que vous savez comment comparer deux dates en Swift, vous êtes prêt à utiliser ces connaissances dans vos projets ! N'hésitez pas à expérimenter avec différentes méthodes de comparaison pour trouver celle qui convient le mieux à votre cas d'utilisation.