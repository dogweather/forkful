---
title:                "Trouver la longueur d'une chaîne de caractères"
html_title:           "Swift: Trouver la longueur d'une chaîne de caractères"
simple_title:         "Trouver la longueur d'une chaîne de caractères"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/swift/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Pourquoi

Peut-être vous vous demandez pourquoi il serait important de trouver la longueur d'une chaîne de caractères. Eh bien, cela peut s'avérer utile dans de nombreuses situations, par exemple lors de la validation de la saisie d'un mot de passe ou d'un numéro de téléphone, ou simplement pour effectuer des calculs.

## Comment faire

Il existe quelques façons de trouver la longueur d'une chaîne de caractères en utilisant Swift. Voici quelques exemples de code et leur sortie correspondante :

```Swift
let message = "Bonjour le monde!"
print(message.count) // affiche : 17
```

```Swift
let email = "john@example.com"
print(email.count) // affiche : 16
```

Comme vous pouvez le voir, la méthode `count` est utilisée pour trouver la longueur d'une chaîne de caractères en Swift.

Vous pouvez également utiliser la propriété `count` sur un tableau pour trouver le nombre d'éléments qu'il contient. Par exemple :

```Swift
let numbers = [1, 2, 3, 4, 5]
print(numbers.count) // affiche : 5
```

Et si vous voulez trouver la longueur d'une chaîne de caractères sans compter les espaces, il existe une méthode `replacingOccurrences()` qui peut vous aider. Voici un exemple :

```Swift
let sentence = "Je suis un texte avec des espaces."
let length = sentence.replacingOccurrences(of: " ", with: "").count
print(length) // affiche : 26
```

## Plongée en profondeur

Maintenant que vous savez comment trouver la longueur d'une chaîne de caractères en utilisant Swift, voici quelques informations supplémentaires à savoir :

- La méthode `count` est sensible à la casse, ce qui signifie qu'elle tiendra compte des majuscules et des minuscules dans la longueur de la chaîne.
- Les caractères spéciaux, tels que les emojis, seront également pris en compte dans la longueur de la chaîne.
- Si vous voulez obtenir le nombre de caractères d'une chaîne plutôt que le nombre d'octets, vous pouvez utiliser la propriété `utf16Count`.
- Il existe d'autres méthodes pour trouver la longueur d'une chaîne de caractères, telles que `distance(from:to:)` et `elementKind(for:)`, qui peuvent être utiles dans certains cas.

Maintenant que vous avez une meilleure compréhension de la façon de trouver la longueur d'une chaîne de caractères en utilisant Swift, vous pouvez l'appliquer dans votre code pour rendre vos applications plus robustes et fiables.

## Voir aussi

- [La documentation officielle de Swift sur les chaînes de caractères](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html)
- [Un guide complet sur les méthodes pour manipuler les chaînes de caractères en Swift](https://learnappmaking.com/swift-string-how-to-how-ignore-case-multiple-strings/)