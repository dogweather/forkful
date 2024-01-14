---
title:    "Swift: Concaténation de chaînes de caractères"
keywords: ["Swift"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/swift/concatenating-strings.md"
---

{{< edit_this_page >}}

## Pourquoi
Concaténer des chaînes de caractères est une étape essentielle de la programmation Swift. Cela nous permet de combiner différentes chaînes pour créer du texte plus complexe. Dans cet article, nous allons explorer comment le faire de manière efficace et comment approfondir cette technique.

## Comment faire
Pour concaténer des chaînes de caractères en Swift, nous pouvons utiliser l'opérateur `+` ou la méthode `append()` de la classe `String`. Voici un exemple de code avec l'utilisation de ces deux méthodes :

```Swift
let nom = "Marie"
let age = 25

// En utilisant l'opérateur +
let presentationAvecOperateur = "Je m'appelle " + nom + " et j'ai " + String(age) + " ans."

// En utilisant la méthode append()
var presentationAvecMethode = "Je m'appelle "
presentationAvecMethode.append(nom)
presentationAvecMethode.append(" et j'ai ")
presentationAvecMethode.append(String(age))
presentationAvecMethode.append(" ans.")
```

Lors de l'utilisation de l'opérateur `+`, il est important de s'assurer que les valeurs à concaténer sont toutes de type `String`. Dans l'exemple ci-dessus, nous devons utiliser `String(age)` pour convertir la variable `age` en chaîne de caractères. La méthode `append()`, quant à elle, peut être appliquée à une variable de type `String` sans avoir besoin de le convertir au préalable.

Voici l'output de ces deux exemples de code :

```
Je m'appelle Marie et j'ai 25 ans.
```

Une autre méthode couramment utilisée pour concaténer des chaînes de caractères est la méthode `join()`. Cette méthode prend en paramètre un séparateur et une liste de chaînes et les concatène en utilisant le séparateur. Voici un exemple :

```Swift
let fruits = ["Pomme", "Banane", "Orange"]

// En utilisant la méthode join()
let listeDeFruits = fruits.join(separator: ", ")

print("Mes fruits préférés sont : \(listeDeFruits).")
```

Output :

```
Mes fruits préférés sont : Pomme, Banane, Orange.
```

## Plongée en profondeur
Il est important de noter que concaténer des chaînes de caractères peut être inefficace pour les grands textes. C'est pourquoi Apple recommande d'utiliser la structure `StringBilder` pour une meilleure performance. Cette structure permet de construire une chaîne de caractères en ajoutant des valeurs à la fin sans avoir à créer une nouvelle instance de `String` à chaque ajout. Voici un exemple :

```Swift
var presentation = StringBuilder()

presentation.append("Je m'appelle ")
presentation.append(nom)
presentation.append(" et j'ai ")
presentation.append(String(age))
presentation.append(" ans.")
```

De plus, pour une meilleure lisibilité de votre code, vous pouvez utiliser la concaténation interlinéaire de Swift en utilisant l'opérateur `+=`. Voici un exemple :

```Swift
var presentation = "Je m'appelle "

presentation += nom
presentation += " et j'ai "
presentation += String(age)
presentation += " ans."
```

## Voir aussi
- [Documentation officielle Swift pour la concaténation de chaînes](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html#ID289)
- [Article sur l'utilisation de la structure StringBilder en Swift] (https://medium.com/swift-india/string-concatenation-in-swift-a-deep-dive-47b0c4370371)