---
title:                "Swift: Suppression de caractères correspondants à un modèle"
simple_title:         "Suppression de caractères correspondants à un modèle"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/swift/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

# Pourquoi supprimer des caractères correspondant à un modèle en Swift ?

Supprimer des caractères correspondant à un modèle peut être utile lorsqu'on travaille avec des chaînes de caractères complexes. Par exemple, si vous avez une chaîne contenant des numéros de téléphone aux formats différents, vous pouvez utiliser cette méthode pour supprimer tous les caractères non numériques et les formater correctement.

## Comment faire

Voici un exemple de code pour supprimer tous les caractères non numériques d'une chaîne :

```Swift
let phoneNumber = "(123) 456-7890"
let numericCharacters = CharacterSet.decimalDigits
let formattedNumber = String(phoneNumber.unicodeScalars.filter(numericCharacters.contains))
print(formattedNumber) // Résultat : 1234567890
```

Dans cet exemple, nous utilisons la propriété `unicodeScalars` de la chaîne pour accéder à chaque caractère et la méthode `filter()` pour ne garder que ceux qui appartiennent à l'ensemble de caractères numériques. Ensuite, nous recréons une nouvelle chaîne à partir des caractères restants.

## Plongée en profondeur

Lorsque vous utilisez cette méthode, il est important de garder à l'esprit que les caractères Unicode sont également pris en compte. Ainsi, si vous avez une chaîne contenant des caractères non-ASCII, tels que des lettres accentuées, ceux-ci seront également supprimés.

De plus, si vous avez besoin de supprimer des caractères spécifiques plutôt que de simplement garder les caractères numériques, vous pouvez le faire en utilisant la méthode `contains()` de `CharacterSet` dans la clause `filter()`.

## Voir aussi

- [Documentation sur la manipulation de chaînes de caractères en Swift](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html)
- [Tutoriel sur les ensembles de caractères en Swift](https://www.hackingwithswift.com/articles/178/super-powered-strings-in-swift-5-1-using-character-set)