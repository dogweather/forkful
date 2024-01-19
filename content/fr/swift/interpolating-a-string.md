---
title:                "Interpolation d'une chaîne de caractères"
html_title:           "Ruby: Interpolation d'une chaîne de caractères"
simple_title:         "Interpolation d'une chaîne de caractères"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/swift/interpolating-a-string.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi?

L'interpolation de chaînes en Swift consiste à incorporer des variables et des expressions dans une chaîne de caractères. C'est utile pour afficher des informations dynamiques dans une chaîne de manière lisible et efficace.

## Comment faire:

Voici un exemple d'interpolation de chaîne en Swift. En utilisant des contre-obliques (\\) et des parenthèses, vous pouvez insérer des variables et des expressions directement dans vos chaînes.

```Swift
var age = 25
var nom = "Jean"
print("Bonjour, \(nom). Vous avez \(age) ans.")
```

Cela donnera en sortie:

```Swift
Bonjour, Jean. Vous avez 25 ans.
```

## Plongée en profondeur:

L'interpolation de chaînes a été introduite en Swift pour remplacer les méthodes traditionnelles de construction de chaînes, comme la concaténation de chaînes, qui peuvent être difficiles à lire et moins efficaces. De plus, ce remplacement conduit à un code plus propre et plus sécurisé.

Quand vous avez des alternatives plus complexes ou de plus grands besoins en formatage d'une chaîne, vous pouvez utiliser des formateurs de chaînes, mais l'interpolation de chaînes reste le moyen le plus utilisé par les Swiftites.

La mise en œuvre de l'interpolation de chaînes est basée sur le protocole `StringInterpolation` qui définit comment les valeurs sont converties en chaînes.

## Voir aussi:

Pour une exploration plus profonde de l'interpolation de chaînes en Swift, consultez ces ressources:

- "La syntaxe de Swift: L'interpolation de chaîne" du blog [SwiftBysundell](https://www.swiftbysundell.com/basics/string-interpolation/)
  
- "L'interpolation de chaîne" dans le [Swift Programming Guide](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html)

- "StringInterpolation" dans la [Swift Standard Library](https://developer.apple.com/documentation/swift/stringinterpolation)