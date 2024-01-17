---
title:                "Concaténation de chaînes"
html_title:           "Swift: Concaténation de chaînes"
simple_title:         "Concaténation de chaînes"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/swift/concatenating-strings.md"
---

{{< edit_this_page >}}

## Qu'est-ce que c'est et pourquoi le faire?

Lorsque vous programmez en Swift, vous pourriez avoir besoin de combiner plusieurs chaînes de caractères en une seule. C'est ce qu'on appelle la concatenation de chaînes. Les programmeurs utilisent cette technique pour créer des phrases dynamiques ou pour formater des données avant de les afficher.

## Comment faire:

Voici un exemple de code en Swift pour concaténer deux chaînes de caractères:

```Swift
let firstName = "Marie"
let lastName = "Dupont"

let fullName = firstName + " " + lastName

print(fullName) // "Marie Dupont"
```

Vous pouvez également utiliser des interpolations de chaînes pour concaténer des variables au sein d'une chaîne:

```Swift
let age = 32
let message = "J'ai \(age) ans!"

print(message) // "J'ai 32 ans!"
```

## Plongée en profondeur:

La concaténation de chaînes existe depuis les débuts de la programmation et est utilisée dans de nombreux langages de programmation. Cependant, Swift offre des fonctionnalités telles que les interpolations de chaînes qui la rendent plus efficace et pratique.

En alternative à la concaténation, certains programmeurs préfèrent utiliser des formateurs de chaînes, tels que `String(format: )`, pour formater leurs données. Cela peut être utile dans certaines situations, mais la concaténation reste un moyen simple et efficace de combiner des chaînes de caractères.

## Voir aussi:

Pour en savoir plus sur la concaténation de chaînes en Swift, vous pouvez consulter la documentation officielle d'Apple sur les interpolations de chaînes [(Interpolations de chaînes en Swift)](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html#ID277). Vous pouvez également trouver des exemples de code sur [cette page GitHub](https://gist.github.com/varunpawnesh/a2812d98b61f231088f132b72aa6fe22).