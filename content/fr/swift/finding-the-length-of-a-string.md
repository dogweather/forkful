---
title:    "Swift: Trouver la longueur d'une chaîne"
keywords: ["Swift"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/swift/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Pourquoi 

La détermination de la longueur d'une chaîne de caractères est une compétence de base pour tout programmeur Swift. Cela vous permet de manipuler efficacement et avec précision les données qui composent vos chaînes. Dans cet article, nous allons explorer la façon de trouver la longueur d'une chaîne de caractères en utilisant Swift.

## Comment faire

Pour trouver la longueur d'une chaîne de caractères en Swift, nous pouvons utiliser la propriété `count` de la structure `String`. Voyons un exemple concret :

```
let string = "Bonjour tout le monde!"

print(string.count) // Output: 21 
```

Comme vous pouvez le voir, la propriété `count` renvoie le nombre de caractères dans la chaîne donnée. Il est important de noter que les espaces comptent également comme des caractères dans cette propriété.

Si vous souhaitez obtenir la longueur d'une chaîne composée de plusieurs éléments (par exemple, un tableau de caractères), vous pouvez utiliser la méthode `count` de la structure `Array` :

```
let characters: [Character] = ["H", "e", "l", "l", "o"]

print(characters.count) // Output: 5
```

## Plongée en profondeur

Il est intéressant de noter que pour les chaînes qui contiennent des caractères Unicode, la propriété `count` renvoie le nombre de caractères, pas le nombre d'octets. Cela est dû au fait que Swift utilise le standard Unicode pour représenter les chaînes de caractères.

De plus, la propriété `count` peut être moins efficace pour les chaînes de caractères très longues, car elle doit parcourir tous les caractères pour les compter. Dans ces cas, il peut être plus rapide d'utiliser la méthode `hasSuffix(_:)` de la structure `String` pour vérifier si une sous-chaîne d'une longueur donnée existe dans la chaîne.

## Voir aussi

- [La documentation officielle de Swift sur la structure String](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html)
- [Un guide complet sur l'utilisation des chaînes de caractères en Swift](https://www.hackingwithswift.com/sixty/3/1/working-with-strings)