---
title:                "Transformation d'une date en chaîne de caractères."
html_title:           "Swift: Transformation d'une date en chaîne de caractères."
simple_title:         "Transformation d'une date en chaîne de caractères."
programming_language: "Swift"
category:             "Swift"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/swift/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Pourquoi

Si vous travaillez avec des dates dans votre code Swift, il est probable que vous ayez besoin de les convertir en chaînes de caractères. Cela peut être utile lorsque vous devez afficher une date dans un format spécifique ou l'utiliser comme identifiant dans une base de données. Dans cet article, nous allons explorer comment convertir une date en une chaîne de caractères en utilisant Swift.

## Comment faire

Pour convertir une date en une chaîne de caractères, nous pouvons utiliser la classe `DateFormatter` de Swift. Cette classe nous permet de formater une date selon différents styles et formats prédéfinis.

### Exemple 1: Conversion simple

Supposons que nous avons une date sous la forme d'un objet `Date` et que nous voulons l'afficher dans un format jour/mois/année. Pour cela, nous pouvons utiliser la méthode `string(from:)` de `DateFormatter` en spécifiant le format souhaité :

```Swift
let date = Date()
// date est maintenant le moment présent

let formatter = DateFormatter()
formatter.dateFormat = "dd/MM/yyyy"

let dateString = formatter.string(from: date)
// dateString sera de la forme "17/07/2021"
```

Dans cet exemple, nous avons créé un objet `DateFormatter` et lui avons attribué un format de date en utilisant la propriété `dateFormat`. Ensuite, nous avons utilisé la méthode `string(from:)` pour convertir notre objet `Date` en une chaîne de caractères selon le format spécifié.

### Exemple 2: Personnalisation du format

Vous pouvez également personnaliser le format de votre date en utilisant des symboles spéciaux dans la propriété `dateFormat` de `DateFormatter`. Par exemple, pour afficher l'heure en plus du jour/mois/année, nous pouvons utiliser le code suivant :

```Swift
formatter.dateFormat = "dd/MM/yyyy HH:mm:ss"
```

Il existe de nombreux symboles disponibles pour formater votre date selon vos besoins, vous pouvez consulter la [documentation officielle de Apple](https://developer.apple.com/documentation/foundation/date_formatter#overview) pour en savoir plus sur ces symboles.

## Plongée en profondeur

Outre le formatage de la date, `DateFormatter` nous permet également de gérer les localisations et les fuseaux horaires. Cela peut être utile si vous avez besoin d'afficher la date dans une langue ou un fuseau horaire spécifique. Voici un exemple de conversion en utilisant un locale différent :

```Swift
let locale = Locale(identifier: "fr-FR")

let frenchFormatter = DateFormatter()
frenchFormatter.locale = locale
frenchFormatter.dateFormat = "dd MMMM yyyy"

let frenchDateString = frenchFormatter.string(from: date)
// frenchDateString sera de la forme "17 juillet 2021"
```

De plus, si vous devez travailler avec des fuseaux horaires, vous pouvez également utiliser la classe `TimeZone` pour spécifier un fuseau horaire différent pour votre date.

## Voir aussi

- [Documentation officielle de Apple pour DateFormatter](https://developer.apple.com/documentation/foundation/date_formatter#overview)
- [Guide de référence de Date et DateFormatter en Swift](https://www.swiftbysundell.com/articles/date-and-date-formatter-in-swift/)
- [Convertisseur de formats de date en ligne](https://nsdateformatter.com/)