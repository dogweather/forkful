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

# Salut les programmeurs Swift !

Bienvenue à tous dans cet article où nous allons parler de la façon de trouver la longueur d'une chaîne de caractères en utilisant Swift. Si vous êtes nouveau dans le monde de la programmation, ne vous inquiétez pas, nous allons tout expliquer de manière simple et directe. Donc, allons droit au but !

## Quoi & Pourquoi ?

Trouver la longueur d'une chaîne de caractères, c'est simplement déterminer combien de caractères composent cette chaîne. Les programmeurs le font souvent pour traiter les données entrantes ou pour vérifier si une chaîne atteint une certaine longueur. Cela peut sembler simple, mais c'est une étape importante dans la gestion des données.

## Comment faire :

Voici un exemple de code pour trouver la longueur d'une chaîne de caractères en utilisant Swift :

```Swift
let phrase = "Bonjour le monde !"
print(phrase.count) // Output : 18
```

Le code ci-dessus déclare une variable contenant une chaîne de caractères et utilise la propriété ```count``` pour trouver sa longueur. Vous pouvez également utiliser la méthode ```count``` sur une chaîne de caractères pour obtenir le même résultat :

```Swift
let phrase = "Bonjour le monde !"
print(phrase.count()) // Output : 18
```

## Plongée en profondeur :

Maintenant, si vous voulez vraiment plonger en profondeur sur la façon de trouver la longueur d'une chaîne de caractères en utilisant Swift, voici quelques informations supplémentaires :

1. Contexte historique : La méthode ```count``` a été introduite dans la version 4 de Swift en remplacement de la méthode obsolète ```character.count```.

2. Alternatives : Vous pouvez également utiliser la méthode ```length``` pour trouver la longueur d'une chaîne de caractères en utilisant Swift.

3. Détails de mise en œuvre : La méthode ```count``` parcourt chaque caractère de la chaîne et retourne le nombre total de caractères, tandis que la méthode ```length``` utilise les informations de codage de la chaîne pour déterminer sa longueur.

## À voir également :

Pour plus d'informations sur la manipulation des chaînes de caractères en Swift, consultez les liens suivants :

- [La documentation officielle de Swift](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html)
- [Le blog de Swift by Sundell](https://www.swiftbysundell.com/articles/strings-in-swift/)