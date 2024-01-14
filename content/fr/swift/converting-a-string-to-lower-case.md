---
title:    "Swift: Transformer une chaîne de caractères en minuscules"
keywords: ["Swift"]
---

{{< edit_this_page >}}

## Pourquoi

Il peut être utile de convertir une chaîne de caractères en minuscules lorsque vous avez besoin de comparer deux chaînes sans tenir compte de la casse ou si vous voulez simplement afficher une chaîne en minuscules pour une meilleure lisibilité.

## Comment faire

La première étape pour convertir une chaîne de caractères en minuscules est d'utiliser la méthode `lowercased()` comme suit :

```Swift
let myString = "Bonjour le Monde!"
let lowercasedString = myString.lowercased()
print(lowercasedString) // affiche "bonjour le monde!"
```

Vous pouvez également utiliser la méthode `folding(options:locale)` pour une conversion plus précise en fonction de la culture et des accents. Par exemple :

```Swift
let frenchString = "Également"
let foldedString = frenchString.folding(options: .diacriticInsensitive, locale: .current)
print(foldedString) // affiche "egalement"
```

## Plongée profonde

Il est important de noter que la conversion en minuscules d'une chaîne de caractères peut être sensible à la langue et à l'encodage utilisé. Par exemple, la lettre "I" peut être différente en minuscules en anglais et en français.

De plus, lorsque vous comparez des chaînes converties en minuscules, assurez-vous de le faire de manière insensible à la casse en utilisant la méthode `caseInsensitiveCompare(_:)`.

## Voir aussi

- [Documentation Apple sur la méthode `lowercased()`](https://developer.apple.com/documentation/foundation/string/1783050-lowercased)
- [Guide programmation Swift en français](https://developer.apple.com/fr/swift/)
- [Forum Stack Overflow sur la conversion de chaîne en minuscules en Swift](https://stackoverflow.com/questions/34459221/convert-string-to-lowercase-in-swift)