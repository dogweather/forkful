---
title:    "Swift: Comparaison de deux dates"
keywords: ["Swift"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/swift/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Pourquoi

Si vous êtes développeur Swift, il est souvent nécessaire de comparer des dates dans votre code. Cela peut être utile pour déterminer si un événement a déjà eu lieu, calculer la durée entre deux dates ou trier des données chronologiquement. Dans cet article, nous allons vous montrer comment comparer efficacement deux dates en Swift.

## Comment Faire

La première étape pour comparer deux dates en Swift est de les convertir en types de données `Date`. Dans l'exemple suivant, nous avons deux dates représentant le 1er janvier 2020 et le 1er juillet 2020 :

```Swift
let date1 = Date(timeIntervalSince1970: 1577836800) // 1 janvier 2020 à minuit
let date2 = Date(timeIntervalSince1970: 1593561600) // 1 juillet 2020 à minuit
```

Maintenant que nous avons défini nos deux dates, nous pouvons utiliser la méthode `compare(_:)` de la classe `Date` pour les comparer. Cette méthode renvoie une énumération `ComparisonResult` qui nous indique si la première date est antérieure, égale ou ultérieure à la seconde.

```Swift
let result = date1.compare(date2)
print(result) // Affiche : .orderedAscending (car date1 est antérieure à date2)
```

Il est également possible de comparer des composants spécifiques d'une date, comme le jour, le mois ou l'année. Pour cela, nous pouvons utiliser la classe `Calendar` pour extraire les composants de nos dates et les comparer.

```Swift
let calendar = Calendar.current
let day1 = calendar.component(.day, from: date1) // 1
let day2 = calendar.component(.day, from: date2) // 1
let result = day1.compare(day2)
print(result) // Affiche : .orderedSame (car les deux dates ont le même jour)
```

## Plongée en Profondeur

Lorsque vous comparez des dates en Swift, il est important de prendre en compte les fuseaux horaires. La classe `Date` stocke les dates sous la forme d'un intervalle de temps en secondes à partir de la date de référence du 1er janvier 1970 à minuit UTC. Cela signifie qu'une date peut être interprétée différemment en fonction du fuseau horaire dans lequel elle est affichée.

Il est donc recommandé d'utiliser la classe `DateComponents` pour comparer des composants de date spécifiques, plutôt que de comparer des dates complètes. Cela garantit que les différences de fuseau horaire ne faussent pas les résultats.

## Voir aussi

- [Documentation officielle de Swift sur la classe `Date`](https://developer.apple.com/documentation/foundation/date)
- [Article de Ray Wenderlich sur la comparaison de dates en Swift](https://www.raywenderlich.com/5522-date-and-calendar-tutorial-for-swift-part-1-nsdate)
- [Tutoriel vidéo de Brian Advent sur la comparaison de dates en Swift](https://www.youtube.com/watch?v=p6bzVjfjm1o)