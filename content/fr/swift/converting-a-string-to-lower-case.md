---
title:                "Swift: Convertir une chaîne en minuscules"
simple_title:         "Convertir une chaîne en minuscules"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/swift/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Pourquoi
Vous vous êtes peut-être déjà demandé pourquoi vous auriez besoin de convertir une chaîne de caractères en minuscules lors de la programmation en Swift. La réponse est simple : cela peut être utile dans de nombreux scénarios, tels que la comparaison de chaînes ou la manipulation d'entrées utilisateur qui peuvent être entrées en majuscules ou en minuscules.

## Comment Faire
Voici un exemple de code en Swift pour convertir une chaîne de caractères en minuscules :
```Swift
let string = "Hola Mon Ami"
let lowercasedString = string.lowercased()
print(lowercasedString)
```
Lorsque vous exécutez ce code, vous obtenez la sortie suivante :
```
hola mon ami
```
Comme vous pouvez le voir, la méthode `lowercased()` a transformé toutes les lettres en minuscules.

## En Profondeur
Lorsqu'il s'agit de convertir une chaîne de caractères en minuscules, il est important de prendre en compte la langue dans laquelle elle est écrite. Par exemple, la lettre "i" en anglais peut être transformée en "ı" en turc lorsqu'elle est en minuscules. Cela peut avoir un impact sur la comparaison de chaînes et doit être pris en compte lors de la manipulation de texte.

De plus, en utilisant la méthode `lowercased()`, les caractères Unicode sont également transformés en minuscules. Cela peut être utile lorsque vous travaillez avec des chaînes multilingues.

## Voir Aussi
- [Documentation officielle de Swift pour la méthode `lowercased()`](https://developer.apple.com/documentation/foundation/nsstring/1411942-lowercased)
- [Article de blog sur la manipulation de chaînes en Swift](https://www.hackingwithswift.com/articles/148/how-to-use-string-interpolation-in-swift)