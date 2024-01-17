---
title:                "Interpoler une chaîne de caractères"
html_title:           "Swift: Interpoler une chaîne de caractères"
simple_title:         "Interpoler une chaîne de caractères"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/swift/interpolating-a-string.md"
---

{{< edit_this_page >}}

## Qu'est-ce que c'est et pourquoi ?
Interpoler une chaîne de caractères est simplement un moyen de remplacer des valeurs spécifiques dans une chaîne de caractères variable. Cela peut être utile pour personnaliser des messages ou pour afficher des données dynamiques dans une interface utilisateur. Les programmeurs le font parce que cela rend le code plus lisible et plus facile à maintenir.

## Comment faire :
Voici un exemple de code Swift :

```Swift
let name = "Marie"
let age = 25
let message = "Bonjour \(name), tu as \(age) ans !"
print(message)
```

L'output de ce code sera :

```
Bonjour Marie, tu as 25 ans !
```

## Plongée en profondeur :
Il existe différentes manières d'interpoler une chaîne de caractères en Swift. L'une des plus courantes est d'utiliser l'opérateur `\( )`, également appelé "insertion de valeurs". Cela permet d'insérer des variables, des expressions ou même des fonctions directement dans une chaîne de caractères. Une autre méthode est d'utiliser la méthode `String(format: )`, qui offre plus de contrôle sur le formatage des données insérées dans la chaîne.

D'un point de vue historique, l'interpolation de chaînes de caractères a été introduite dans Swift 1.0, en 2014. Avant cela, les développeurs utilisaient principalement la méthode `stringWithFormat` de l'API Objective-C pour interpoler des chaînes.

Il existe également des alternatives à l'interpolation de chaînes en Swift, telles que l'utilisation de `NSAttributedString` ou la concaténation de chaînes avec l'opérateur `+` ou la méthode `stringByAppendingString`.

## À voir également :
- La documentation officielle de Swift sur l'interpolation de chaînes : https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html#ID284
- Un tutoriel sur l'interpolation de chaînes en Swift : https://www.raywenderlich.com/179924/intermediate-swift-tutorial-interpolation-strings
- Un exemple pratique d'interpolation de chaînes en utilisant SwiftUI : https://www.hackingwithswift.com/quick-start/swiftui/how-to-interpolate-strings-using-swiftui Text views