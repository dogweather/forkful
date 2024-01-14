---
title:                "Swift: Obtenir la date actuelle"
programming_language: "Swift"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/swift/getting-the-current-date.md"
---

{{< edit_this_page >}}

# Pourquoi

Il y a plusieurs raisons pour lesquelles on peut vouloir obtenir la date actuelle dans un programme Swift. Que ce soit pour afficher la date sur une interface utilisateur, pour calculer un délai ou simplement pour des besoins de logistique, connaître la date peut être utile dans de nombreuses situations.

# Comment faire

Voici deux façons d'obtenir la date actuelle dans Swift, en utilisant les classes `Date` et `DateFormatter`.

\`\`\`Swift
// Option 1 : Utiliser la classe Date
let date = Date() // Cela créé une instance de Date qui contient la date et l'heure actuelles
print(date) // Affiche la date au format standard : 
// 2020-06-15 14:30:00 +0000

// Option 2 : Utiliser la classe DateFormatter pour formater l'affichage
let dateFormatter = DateFormatter()
dateFormatter.dateStyle = .long
dateFormatter.timeStyle = .none
let formattedDate = dateFormatter.string(from: date) // Formate la date selon le style choisi
print(formattedDate) // Affiche simplement la date : 
// 15 juin 2020

\`\`\`

La classe `DateFormatter` offre également d'autres options de formatage, telles que la langue, le fuseau horaire, et le format d'affichage des heures et des minutes.

# Plongeons plus en profondeur

Maintenant que nous avons vu comment obtenir la date actuelle en Swift, il est important de comprendre que cette date est en fait un objet `Date` qui représente un point spécifique dans le temps. Elle peut être convertie en d'autres fuseaux horaires, comparée à d'autres dates, et utilisée pour exécuter des opérations mathématiques.

Il est également important de noter que la classe `Date` utilise le fuseau horaire UTC (temps universel coordonné), donc il peut être nécessaire de la convertir en utilisant `DateFormatter` pour afficher la date dans le fuseau horaire local de l'utilisateur.

# Voir aussi

- [Documentation officielle sur la classe Date](https://developer.apple.com/documentation/foundation/date)
- [Guide Swift sur les dates et les heures](https://swift.org/blog/building-better-deeper-and-faster-apis-in-swift/)
- [Site de référence pour DateFormatter](https://nsdateformatter.com/)