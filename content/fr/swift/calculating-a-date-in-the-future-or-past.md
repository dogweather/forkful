---
title:    "Swift: Calculer une date dans le futur ou le passé"
keywords: ["Swift"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/swift/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Pourquoi
Calculer une date dans le futur ou le passé peut être utile pour de nombreuses raisons, telles que la planification d'événements, la gestion de projets ou même simplement par curiosité.

## Comment faire
Pour calculer une date dans le futur ou le passé en utilisant Swift, nous pouvons utiliser la classe `Date` et les méthodes `Calendar` pour effectuer des calculs. Voici un exemple de code qui calcule la date dans 1 mois à partir de la date actuelle :

```Swift
let currentDate = Date()
let calendar = Calendar.current
let futureDate = calendar.date(byAdding: .month, value: 1, to: currentDate)
print(futureDate)
```

Cela affichera la date dans un format standard, tel que `2021-04-20 13:00:00 +0000`. Nous pouvons également spécifier un format personnalisé en utilisant `DateFormatter`.

Pour calculer une date dans le passé, nous pouvons utiliser la méthode `date(byAdding:to:wrappingComponents:)` et spécifier un nombre négatif pour la valeur, comme ceci :

```Swift
let pastDate = calendar.date(byAdding: .year, value: -2, to: currentDate)
print(pastDate)
```

Cela nous donnera la date d'il y a 2 ans à partir de la date actuelle.

## Plongée en profondeur
En plus des méthodes `Calendar`, Swift offre également des options pour spécifier plus précisément une date dans le futur ou le passé en utilisant la classe `DateComponents`. Par exemple, nous pouvons spécifier une date en utilisant les composants suivants : année, mois, jour, heure, minute, seconde, nanoseconde et fuseau horaire.

De plus, nous pouvons également utiliser la méthode `date(from:)` pour convertir une chaîne de caractères en une date. Cela peut être utile si nous recevons une date sous forme de texte et que nous voulons effectuer des calculs avec.

## Voir aussi
- [Documentation Apple sur la classe `Date`](https://developer.apple.com/documentation/foundation/date)
- [Documentation Apple sur la classe `Calendar`](https://developer.apple.com/documentation/foundation/calendar)
- [Documentation Apple sur la classe `DateComponents`](https://developer.apple.com/documentation/foundation/datecomponents)
- [Article sur le calcul de dates dans Swift](https://www.hackingwithswift.com/articles/141/how-to-work-with-dates-and-times-in-swift)