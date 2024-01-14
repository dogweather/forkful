---
title:                "Swift: Obtenir la date actuelle"
simple_title:         "Obtenir la date actuelle"
programming_language: "Swift"
category:             "Swift"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/swift/getting-the-current-date.md"
---

{{< edit_this_page >}}

# Pourquoi

Si vous êtes un développeur Swift, il y a de fortes chances que vous deviez à un moment donné récupérer la date actuelle dans votre code. Cela peut être utile pour afficher la date pour l'utilisateur, enregistrer la date et l'heure d'une action ou pour toute autre tâche liée à la manipulation de la date. Dans cet article, nous allons vous montrer comment récupérer la date actuelle en utilisant Swift.

## Comment faire

Pour récupérer la date actuelle, nous allons utiliser la classe `Date`. Voici un exemple de code qui montre comment obtenir la date actuelle et l'imprimer dans la console :

```swift
let currentDate = Date()
print(currentDate)
// Output: 2021-03-15 10:30:00 +0000
```

Comme vous pouvez le voir, la date est affichée au format ISO 8601 par défaut. Si vous souhaitez afficher la date dans un format différent, vous pouvez utiliser un `DateFormatter` pour le formater selon vos besoins. Par exemple, pour afficher la date au format "dd/MM/yyyy", vous pouvez utiliser le code suivant :

```swift
let dateFormatter = DateFormatter()
dateFormatter.dateFormat = "dd/MM/yyyy"
let currentDate = Date()
print(dateFormatter.string(from: currentDate))
// Output: 15/03/2021
```

Il est également possible d'obtenir la date actuelle sous forme de chaîne de caractères avec le même format en utilisant la méthode `string()` de `DateFormatter` :

```swift
let dateFormatter = DateFormatter()
dateFormatter.dateFormat = "dd/MM/yyyy"
let currentDate = Date()
let currentDateAsString = dateFormatter.string(from: currentDate)
print(currentDateAsString)
// Output: 15/03/2021
```

## Plongée en profondeur

Pour comprendre comment fonctionne la récupération de la date actuelle en Swift, il est important de comprendre le concept de temps universel coordonné (UTC). En informatique, le temps est généralement stocké sous forme de nombre de secondes écoulées depuis le 1er janvier 1970 à 00:00:00 UTC. Lorsque nous récupérons la date actuelle avec `Date()`, nous obtenons le nombre de secondes écoulées depuis cette date. Cela signifie que la date et l'heure que nous obtenons dépendent du fuseau horaire de l'appareil sur lequel le code est exécuté.

Il est également important de noter que la classe `Date` représente un instant précis dans le temps et ne contient aucune information sur les fuseaux horaires. La conversion d'un `Date` en chaîne de caractères dépend du système par défaut de l'appareil. Cela signifie que si votre application est utilisée dans différents fuseaux horaires, la date récupérée peut être différente selon l'emplacement.

# Voir aussi

- [Documentation sur la classe Date en Swift](https://developer.apple.com/documentation/foundation/date)
- [Tutoriel sur la manipulation des dates en Swift](https://www.raywenderlich.com/1388445-dates-and-times-in-swift)