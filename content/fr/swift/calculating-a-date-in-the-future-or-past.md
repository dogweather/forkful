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

## Pourquoi

Il arrive souvent que nous ayons besoin de calculer une date dans le passé ou le futur dans le cadre de notre programmation Swift. Cela peut être utile pour des tâches telles que la planification d'événements ou la manipulation de données temporelles.

## Comment faire

Pour calculer une date dans le futur, nous pouvons utiliser la fonction `DateAddingTimeInterval` avec le nombre de secondes correspondant à la période de temps que nous voulons ajouter. Par exemple, pour ajouter un jour à la date actuelle, nous pouvons utiliser le code suivant:

```
let currentDate = Date()
let futureDate = currentDate.addingTimeInterval(86400) //86400 secondes = 1 jour
print(futureDate) // sortie: 2021-09-27 16:00:00 +0000
```

De même, pour calculer une date dans le passé, nous pouvons utiliser la fonction `DateByAddingTimeInterval` en soustrayant le nombre de secondes de la date actuelle. Voici un exemple de code pour soustraire un mois à la date actuelle:

```
let currentDate = Date()
let pastDate = currentDate.addingTimeInterval(-2629746) //2629746 secondes = 1 mois
print(pastDate) // sortie: 2021-07-29 16:00:00 +0000
```

Il est important de noter que tous les calculs de dates dépendent du fuseau horaire sur lequel votre appareil est réglé. Assurez-vous donc de prendre cela en compte lors de l'utilisation de ces fonctions.

## Plongée en profondeur

Lorsque nous utilisons la fonction `addintTimeInterval` pour calculer une date dans le futur ou dans le passé, nous devons fournir le nombre de secondes correspondant à la période de temps que nous voulons ajouter ou soustraire. Mais comment pouvons-nous calculer facilement le nombre de secondes pour une période donnée?

Heureusement, Swift a une solution pour cela: le type `Calendar`. En utilisant cette classe, nous pouvons obtenir des composants de temps spécifiques, tels que le nombre de jours, de mois ou d'années, et les convertir en secondes pour les utiliser dans nos calculs de date.

Par exemple, si nous voulons calculer une date dans 2 semaines à partir de maintenant, nous pouvons utiliser le code suivant:

```
let calendar = Calendar.current
let twoWeeksInSeconds = calendar.dateComponents([.second], from: Date(), to: calendar.date(byAdding: .day, value: 14, to: Date()))
print(twoWeeksInSeconds?.second) // sortie: 1209600 secondes (14 jours)
```

En utilisant cette méthode, nous pouvons facilement personnaliser nos calculs de dates en fonction de nos besoins.

## Voir aussi

Pour plus d'informations sur la manipulation de dates en Swift, vous pouvez consulter les ressources suivantes:

- [Documentation Swift sur la classe Date](https://developer.apple.com/documentation/foundation/date)
- [Tutoriel sur la manipulation de dates avec le type Calendar](https://www.hackingwithswift.com/example-code/system/how-to-convert-dates-and-times-to-a-string-using-dateformatter)
- [Vidéo explicative sur les fonctions de manipulation de dates en Swift](https://www.youtube.com/watch?v=_4cXCEDtpLU)