---
title:                "Swift: Concaténation de chaînes de caractères"
programming_language: "Swift"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/swift/concatenating-strings.md"
---

{{< edit_this_page >}}

## Pourquoi

La concaténation de chaînes de caractères est une technique utile en programmation Swift pour combiner plusieurs chaînes en une seule. Cela peut être utile pour afficher du texte dans une interface utilisateur ou pour créer des messages personnalisés.

## Comment faire

Voici un exemple de code montrant comment concaténer des chaînes de caractères en utilisant l'opérateur "+" :

```Swift
let firstName = "Jean"
let lastName = "Dupont"
let fullName = firstName + " " + lastName
print(fullName)
```

Cela produira l'output :

```
Jean Dupont
```

Un autre moyen de concaténer des chaînes de caractères est d'utiliser la méthode `String(format: )`, comme ceci :

```Swift
let message = String(format: "Bonjour %@, comment ça va aujourd'hui ?", firstName)
print(message)
```

Et cela produira :

```
Bonjour Jean, comment ça va aujourd'hui ?
```

Vous pouvez également utiliser des interpolations de chaînes pour incorporer des variables dans une chaîne de caractères, comme ceci :

```Swift
let temperature = 23
let weatherString = "Il fait \(temperature) degrés aujourd'hui."
print(weatherString)
```

Ce qui donnera :

```
Il fait 23 degrés aujourd'hui.
```

En utilisant ces techniques, vous pouvez facilement concaténer des chaînes de caractères selon vos besoins.

## Deep Dive

En plus des opérateurs et des méthodes mentionnés ci-dessus, il existe d'autres façons de concaténer des chaînes de caractères en Swift. Par exemple, vous pouvez utiliser la méthode `joined(separator: )` sur un tableau de chaînes de caractères pour les combiner en une seule :

```Swift
let fruits = ["pomme", "banane", "orange"]
let fruitString = fruits.joined(separator: ", ")
print("Je vais manger une \(fruitString) pour le petit-déjeuner.")
```

Ceci produira :

```
Je vais manger une pomme, banane, orange pour le petit-déjeuner.
```

Il est également possible d'utiliser des littéraux de chaînes multilignes pour concaténer du texte sur plusieurs lignes :

```Swift
let slogan = """
Le meilleur moyen de prédire l'avenir, c'est de le créer.
- Peter Drucker
"""
```

Enfin, en utilisant les caractères spéciaux `\n` et `\t`, vous pouvez formatter vos chaînes de caractères pour une meilleure lisibilité :

```Swift
let address = """
88 rue de la Liberté
75010 Paris
\r\n
\tFrance
"""
```

Et cela produira l'output suivant :

```
88 rue de la Liberté
75010 Paris

      France
```

## Voir aussi

Découvrez-en plus sur les chaînes de caractères et d'autres concepts de programmation Swift en consultant ces ressources utiles :

- [Guide de démarrage avec Swift d'Apple](https://developer.apple.com/library/archive/documentation/Swift/Conceptual/Swift_Programming_Language/StringsAndCharacters.html)
- [Documentation officielle sur les chaînes de caractères en Swift](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html)
- [Tutoriels interactifs de Swift Playgrounds](https://www.apple.com/swift/playgrounds/)