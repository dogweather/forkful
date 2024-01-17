---
title:                "Convertir une date en chaîne de caractères"
html_title:           "Swift: Convertir une date en chaîne de caractères"
simple_title:         "Convertir une date en chaîne de caractères"
programming_language: "Swift"
category:             "Swift"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/swift/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

##Qu'est-ce que c'est et pourquoi le faire?

Convertissez une date en chaîne de caractères n'est rien d'autre que le fait de prendre une date sous forme de données et de la transformer en une représentation lisible pour les humains. Les programmeurs le font souvent pour afficher des dates dans des formats spécifiques ou pour les comparer.

##Comment faire:

Voici quelques exemples de code en Swift pour vous montrer comment convertir une date en chaîne de caractères :

```
//Convertir une date en une chaîne de caractères dans un format spécifique
let date = Date()
let formatter = DateFormatter()
formatter.dateFormat = "dd.MM.yyyy"
let stringDate = formatter.string(from: date)
//Output: "29.06.2021"

//Convertir une date en chaîne de caractères en utilisant le format prédéfini du système
let isoFormatter = ISO8601DateFormatter()
isoFormatter.string(from: date)
//Output: "2021-06-29T11:52:52+0000"

//Convertir une date en chaîne de caractères en utilisant un style relatif
let relativeFormatter = RelativeDateTimeFormatter()
relativeFormatter.localizedString(for: date, relativeTo: Date())
//Output: "il y a quelques secondes"
```

##Plongeons plus en profondeur:

Convertir des dates en chaînes de caractères est une tâche courante pour les programmeurs, mais cela peut être délicat si vous devez gérer différentes langues, fuseaux horaires et formats de date. Heureusement, Swift offre plusieurs options avec les classes DateFormatter, DateComponentsFormatter et RelativeDateTimeFormatter pour faciliter la tâche. Alternativement, vous pouvez également stocker des dates en tant que chaînes de caractères dans une base de données ou utiliser des bibliothèques tierces pour faciliter la conversion.

##Voir aussi:

- [Documentation Apple pour DateFormatter](https://developer.apple.com/documentation/foundation/dateformatter)
- [Exemples de DateFormatter en Swift](https://learnappmaking.com/dateformatter-swift-how-to/)
- [Guide de manipulation de dates en Swift](https://www.swiftbysundell.com/basics/dates/)