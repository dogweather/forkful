---
title:                "Swift: Extraction de sous-chaînes"
simple_title:         "Extraction de sous-chaînes"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/swift/extracting-substrings.md"
---

{{< edit_this_page >}}

#Pourquoi

Les sous-chaînes sont un élément essentiel de la programmation Swift. Elles permettent de découper et d'extraire des parties spécifiques d'une chaîne de caractères, ce qui est particulièrement utile pour manipuler et traiter des données. Apprenez à utiliser les sous-chaînes pour rendre votre code plus efficace et plus précis.

##Comment Faire

Voici un exemple de code en Swift pour extraire une sous-chaîne à partir d'une chaîne de caractères:

```Swift
let nomComplet = "Jean Dupont"
let prenom = nomComplet.prefix(4)
let nom = nomComplet.suffix(6)

print(prenom) // Jean
print(nom) // Dupont
```

Dans cet exemple, nous utilisons les méthodes `prefix()` et `suffix()` pour extraire les premiers et derniers caractères de la chaîne `nomComplet`. Vous pouvez également utiliser la méthode `subscript()` pour extraire une portion spécifique d'une chaîne en spécifiant une plage de caractères. Par exemple:

```
let phrase = "Bonjour tout le monde!"
let texte = phrase[7...10]

print(texte) // tout
```

N'hésitez pas à expérimenter avec ces méthodes pour découvrir toutes les possibilités qu'elles offrent.

##Plongée en Profondeur

Lorsque vous travaillez avec des sous-chaînes, il est important de comprendre le concept de `Substring` en Swift. Les sous-chaînes sont en fait des vues sur une partie de la chaîne d'origine, plutôt que de nouvelles chaînes indépendantes. Cela signifie que les sous-chaînes peuvent partager des éléments de données avec la chaîne d'origine, ce qui peut parfois entraîner des erreurs si vous n'êtes pas conscient de cela.

De plus, il est possible de modifier la chaîne d'origine tout en travaillant avec des sous-chaînes, ce qui peut également entraîner des problèmes. Assurez-vous d'utiliser la méthode `String()` pour convertir une sous-chaîne en une nouvelle chaîne indépendante si nécessaire.

#Voir Aussi

- [La documentation officielle de Swift sur les sous-chaînes](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html#ID297)
- [Un tutoriel sur les sous-chaînes en Swift](https://www.hackingwithswift.com/example-code/strings/how-to-extract-a-substring-from-a-string)
- [Une explication approfondie sur les sous-chaînes en Swift](https://www.avanderlee.com/swift/substrings-swift/)