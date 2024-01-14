---
title:    "Swift: Convertir une date en chaîne de caractères"
keywords: ["Swift"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/swift/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Pourquoi
Si vous êtes un développeur Swift, vous êtes probablement familiarisé avec le type `Date` qui représente une date et une heure spécifiques. Si vous avez déjà travaillé avec les dates, vous avez peut-être eu besoin de les convertir en chaînes de caractères (strings) pour les utiliser dans votre application. Dans cet article, nous allons examiner comment convertir une date en une chaîne de caractères en utilisant le langage Swift.

## Comment faire
La méthode la plus simple pour convertir une date en une chaîne de caractères consiste à utiliser le `DateFormatter` de Swift. Tout d'abord, nous allons créer une instance de `DateFormatter`, qui est une classe qui nous permet de formater les dates en chaînes de caractères selon nos besoins.

```Swift
let dateFormatter = DateFormatter()
```

Ensuite, nous devons spécifier le format dans lequel nous voulons que notre date soit affichée. Par exemple, pour afficher la date en format "jour/mois/année", nous pouvons utiliser la ligne suivante :

```Swift
dateFormatter.dateFormat = "dd/MM/yyyy"
```

Une fois que nous avons choisi notre format, nous pouvons utiliser la méthode `string(from: )` pour convertir notre date en une chaîne de caractères.

```Swift
let dateString = dateFormatter.string(from: Date())
```

Si nous imprimons `dateString`, nous obtiendrons une chaîne de caractères représentant la date actuelle dans le format que nous avons spécifié. Par exemple : "25/09/2021".

## Deep Dive
En plus du format de date standard `dateFormat`, le `DateFormatter` de Swift offre également d'autres options pour formater les dates en différentes langues et styles. Par exemple, vous pouvez spécifier un style court, moyen ou long pour la date et l'heure en utilisant les propriétés `dateStyle` et `timeStyle` du `DateFormatter`. Vous pouvez également personnaliser le format en utilisant des modèles comme "EEE" pour le nom abrégé du jour de la semaine ou "MMM" pour le nom abrégé du mois.

```Swift
dateFormatter.dateStyle = .medium
dateFormatter.timeStyle = .none 

//Output : Sep 25, 2021
```

Vous pouvez également utiliser les `locale` pour spécifier une région spécifique et afficher la date et l'heure dans le format local de cette région.

```Swift
dateFormatter.locale = Locale(identifier: "fr_FR") 

//Output: 25 sept. 2021
```

## Voir aussi
- [Apple Developer Documentation](https://developer.apple.com/documentation/foundation/dateformatter)
- [Stack Overflow - Formatting Dates with DateFormatter in Swift](https://stackoverflow.com/questions/35700281/formatting-dates-with-dateformatter-in-swift)
- [Swift Education - How to work with dates in Swift](https://www.swifteducation.com/swift/working-with-dates-in-swift/)