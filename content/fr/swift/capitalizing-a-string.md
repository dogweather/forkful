---
title:    "Swift: Majusculiser une chaîne de caractères"
keywords: ["Swift"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/swift/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Pourquoi

Le capitalisation d'une chaîne de caractères est un aspect important de la programmation Swift car elle permet de mettre en évidence certaines parties du texte ou de respecter certaines normes de présentation. Dans cet article, nous allons explorer comment capitaliser une chaîne de caractères en utilisant le langage Swift.

## Comment faire

Pour capitaliser une chaîne de caractères en Swift, nous pouvons utiliser la méthode `capitalized`. Voici un exemple de code avec une chaîne de caractères en minuscules :

```Swift
let string = "hello world"
let capitalizedString = string.capitalized
print(capitalizedString)

// Output : "Hello World"
```

Nous pouvons également utiliser la méthode `uppercased` pour capitaliser tous les caractères d'une chaîne, même ceux qui sont déjà en majuscules :

```Swift
let string = "HeLLo wORld"
let capitalizedString = string.uppercased()
print(capitalizedString)

// Output : "HELLO WORLD"
```

Si nous voulons capitaliser uniquement le premier mot d'une chaîne, nous pouvons utiliser la méthode `capitalizingFirstLetter` en combinaison avec `lowercased` :

```Swift
let string = "apple banana orange"
let firstWordCapitalized = string.capitalizingFirstLetter()
print(firstWordCapitalized)

// Output : "Apple banana orange"

let onlyFirstWordCapitalized = firstWordCapitalized.lowercased()
print(onlyFirstWordCapitalized)

// Output : "apple banana orange"
```

## Plongée en profondeur

En explorant les méthodes de capitalisation en Swift, nous pouvons constater que l'utilisation de `capitalized` est plus efficace que d'utiliser `uppercased` car elle prend en compte les caractères spéciaux et les accents.

Par exemple, avec la chaîne de caractères "éééé", `uppercased` retournerait la même chaîne alors que `capitalized` la capitaliserait correctement en "Éééé".

## Voir aussi

Vous pouvez en apprendre davantage sur la capitalisation des chaînes de caractères en Swift en consultant les liens suivants :

- [La documentation officielle de Swift sur les méthodes de capitalisation](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html#ID538)
- [Un tutoriel pour débutants sur la manipulation des chaînes de caractères en Swift](https://www.ralfebert.de/ios-examples/foundation/string/uikit_nsstring_nsattributedstring/capitalize/)
- [Un article expliquant comment capitaliser correctement les noms propres en Swift](https://www.hackingwithswift.com/example-code/strings/how-to-capitalize-the-first-letter-of-a-string)

Maintenant que vous maîtrisez la capitalisation en Swift, vous pouvez faciliter la présentation de vos textes dans vos applications et respecter les normes de présentation. Bon codage !