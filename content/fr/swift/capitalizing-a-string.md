---
title:                "Majuscule d'une chaîne de caractères"
html_title:           "Swift: Majuscule d'une chaîne de caractères"
simple_title:         "Majuscule d'une chaîne de caractères"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/swift/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Qu'est-ce que la capitalisation d'un string et pourquoi les programmeurs le font-ils?

La capitalisation d'un string fait référence au fait de mettre en majuscules la première lettre d'un mot ou d'une phrase. Elle est souvent utilisée pour améliorer la lisibilité et la cohérence dans un programme. Par exemple, en capitalisant les noms de variables et de fonctions, il est plus facile de distinguer ces éléments du reste du code.

## Comment faire:

Voici deux manières courantes de capitaliser un string en Swift:

```Swift
// Utiliser la méthode capitalize() sur une String pour capitaliser la première lettre
let name = "pierre"
print(name.capitalize())  // "Pierre"

// Utiliser la méthode uppercased() sur une String pour mettre en majuscules toutes les lettres
let message = "bonjour tout le monde"
print(message.uppercased())  // "BONJOUR TOUT LE MONDE"
```

## Plongée en profondeur:

La capitalisation des strings est une pratique courante dans de nombreux langages de programmation. Elle aide à rendre le code plus lisible et à éviter les erreurs en distinguant clairement les différents éléments. De plus, en utilisant des conventions de capitalisation cohérentes, il est plus facile de travailler en équipe sur un projet.

Il existe également d'autres méthodes pour capitaliser un string en Swift, telles que `capitalized(with:)`, qui permet de spécifier une locale pour la capitalisation. De plus, certains programmeurs préfèrent utiliser des bibliothèques externes dédiées à la manipulation des strings, telles que SwiftString ou Stringly.

## Voir aussi:

- [Documentation officielle de Swift sur la manipulation des strings](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html)
- [SwiftString - une bibliothèque open-source pour la manipulation de strings en Swift](https://github.com/amayne/SwiftString)
- [Stringly - une autre bibliothèque dédiée à la manipulation des strings en Swift](https://github.com/nvzqz/Stringly)