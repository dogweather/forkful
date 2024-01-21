---
title:                "Trouver la longueur d'une chaîne de caractères"
date:                  2024-01-20T17:48:32.384449-07:00
model:                 gpt-4-1106-preview
simple_title:         "Trouver la longueur d'une chaîne de caractères"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/swift/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Quoi et Pourquoi ? 
Trouver la longueur d'une chaîne signifie compter le nombre de caractères qui la composent. Les programmeurs le font pour valider des saisies, optimiser des performances ou simplement manipuler des données textuelles.

## Comment faire :
```swift
let salutation = "Bonjour"
print("La longueur de la chaîne est \(salutation.count).")
```
Sortie :
```
La longueur de la chaîne est 7.
```

Un autre exemple, avec des emojis qui sont traités comme des caractères uniques en Swift:
```swift
let salutationAvecEmoji = "Salut 👋"
print("La longueur de la chaîne est \(salutationAvecEmoji.count).")
```
Sortie :
```
La longueur de la chaîne est 7.
```

## Plongée Profonde
Historiquement, la façon de compter les caractères pouvait varier avec les anciennes versions de Swift. En effet, avant Swift 2.0, les programmeurs utilisaient la méthode `length` de `NSString`. Maintenant, `String` en Swift a la propriété `.count` qui tient compte des caractères composés et des emojis correctement grâce à la conformité avec Unicode.

Les alternatives, comme la parcourir avec un `for-in` loop, sont peu pratiques et lourdes. Swift traite chaque `Character` comme un graphème étendu, ce qui veut dire que même les séquences complexes sont correctement comptées comme un seul caractère.

Pour savoir la longueur d'une chaîne, Swift doit effectuer une opération coûteuse car il compte les graphèmes étendus tous les caractères Unicode représenter peuvent compter pour plus d'un point de code.

## Voir Aussi
- Documentation officielle de Swift sur les chaînes : [Strings and Characters](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html)
- Un article détaillant `String` en Swift: [Swift String](https://www.hackingwithswift.com/articles/181/understanding-swifts-string-2)
- Pour comprendre les graphèmes étendus : [Extended Grapheme Clusters](https://unicode.org/reports/tr29/#Grapheme_Cluster_Boundaries)