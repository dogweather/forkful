---
title:                "Convertir une date en chaîne de caractères"
html_title:           "Gleam: Convertir une date en chaîne de caractères"
simple_title:         "Convertir une date en chaîne de caractères"
programming_language: "Swift"
category:             "Swift"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/swift/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi?
Transformer une date en chaîne de caractères, c'est convertir une valeur de type `Date` en `String`. Cette opération est utile dans plusieurs cas, notamment pour afficher une date au format souhaité à l'utilisateur ou pour la stocker dans une base de données.

## Comment faire :

Voici un exemple de comment vous pouvez procéder dans Swift :
```Swift 
import Foundation

let maintenant = Date()
let formatteur = DateFormatter()
formatteur.dateFormat = "yyyy-MM-dd HH:mm:ss"
let chaineDate = formatteur.string(from: maintenant)

print(chaineDate)
```

Dans cet exemple, la sortie sera similaire à `2022-03-31 10:42:16`.

## Plongeon plus profond :

Historiquement, la conversion des dates en chaînes a été intégrée dans les premiers langages de programmation pour permettre une communication plus facile avec les utilisateurs. En Swift, cette opération est grandement facilitée grâce à la classe `DateFormatter`.

Il existe des alternatives à cette méthode. Par exemple, avec la classe `ISO8601DateFormatter` pour un format de date universel. 
```Swift 
let maintenant = Date()
let formatteur = ISO8601DateFormatter()
let chaineDate = formatteur.string(from: maintenant)

print(chaineDate)
```
La sortie sera ici de la forme `2022-03-31T10:42:16Z`.

Et n'oubliez pas, lors de l'utilisation des formatteurs de date, tenez compte du paramètre des régions (`Locale`). Le format de la date sera différent selon que vous êtes en France, aux États-Unis, etc.

## Voir aussi :

Pour plus de détails et d'exemples sur la conversion de `Date` en `String` dans Swift, vous pouvez consulter les ressources suivantes :

1. Documentation Apple sur [`DateFormatter`](https://developer.apple.com/documentation/foundation/dateformatter)

2. Documentation Apple sur [`ISO8601DateFormatter`](https://developer.apple.com/documentation/foundation/iso8601dateformatter)

3. Guide pratique pour gérer les dates et les temps dans Swift : [https://www.hackingwithswift.com/articles/141/working-with-dates-and-times-in-swift](https://www.hackingwithswift.com/articles/141/working-with-dates-and-times-in-swift)