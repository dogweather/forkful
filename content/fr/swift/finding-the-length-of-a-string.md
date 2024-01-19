---
title:                "Trouver la longueur d'une chaîne"
html_title:           "Go: Trouver la longueur d'une chaîne"
simple_title:         "Trouver la longueur d'une chaîne"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/swift/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?

Trouver la longueur d'une chaîne consiste à déterminer le nombre de caractères contenus dans cette chaîne. Les programmeurs le font souvent pour manipuler ou valider les données en fonction de leur longueur.

## Comment Faire :

Voici comment vous pouvez trouver la longueur d'une chaîne en Swift :

```Swift
let chaine = "Bonjour le monde"
print("La longueur de la chaîne est \(chaine.count)")
```

Dans le code ci-dessus, `.count` est une propriété de l'objet String qui nous donne le nombre de caractères. L'échantillon produira cette sortie :

```
La longueur de la chaîne est 16
```
## Plongée Profonde 

Historiquement, Swift n'a pas toujours eu `.count` pour calculer la longueur d'une chaîne. Les versions antérieures de Swift utilisaient la méthode `countElements()`. Voici comment l'ancien code aurait pu ressembler :

```Swift
let chaine = "Bonjour le monde"
print("La longueur de la chaîne est \(countElements(chaine))")
```

Il est aussi possible de transformer la chaîne en tableau de caractères et d'en calculer la longueur avec `.count`. C'est une alternative valide, bien que la performance soit généralement inférieure à `.count`. 

Dans les coulisses, quand un programmeur appelle `.count` sur une chaîne, Swift utilise une optimisation intelligente pour faire le calcul de façon efficace, peu importe la longueur de la chaîne, même avec des chaînes Unicode complexes.

## Voir Aussi:

- Documentation officielle de Swift sur les chaînes : [Strings and Characters in Swift](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html)
- Guide sur les chaînes en Swift par NSHipster : [NSHipster String](https://nshipster.com/string/)
- Swift par Sundell sur les propriétés et méthodes de String : [Swift by Sundell: Strings](https://www.swiftbysundell.com/posts/strings-in-swift)