---
title:                "Mettre une chaîne de caractères en majuscules"
html_title:           "C: Mettre une chaîne de caractères en majuscules"
simple_title:         "Mettre une chaîne de caractères en majuscules"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/swift/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?
Capitaliser une chaîne de caractères, c'est transformer toutes les lettres en majuscules. Ça sert à standardiser les textes, pour les titres ou pour s'assurer que les mots-clés soient cohérents malgré la variété des saisies.

## Comment faire :
Voici comment capitaliser une chaîne en Swift :

```Swift
let smallText = "bonjour, comment ça va?"
let capitalizedText = smallText.uppercased()
print(capitalizedText) // "BONJOUR, COMMENT ÇA VA?"
```

Très simple, non ? 

## Exploration approfondie
Historiquement, la capitalisation remonte aux manuscrits médiévaux, pour accentuer ou distinguer des sections. Dans les programmes informatiques, on capitalise pour des raisons semblables : lisibilité et conformité.

Alternativement, vous voudrez peut-être seulement mettre en majuscule la première lettre de chaque mot (capitalisation de titre) :

```Swift
let title = "le seigneur des anneaux"
let titleCased = title.capitalized // "Le Seigneur Des Anneaux"
```

Il est important de noter que `uppercased()` et `capitalized` tiennent compte de la locale par défaut. Cela peut affecter le comportement des opérations de majuscule dans certaines langues.

Les détails d'implémentation sont ajustés pour être efficaces et insensibles à la casse, mais méfiez-vous des implications de performance sur de très grandes strings.

## Voir également
- [Documentation Swift sur les strings](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html)
- [Guide de style Unicode pour la capitalisation](http://www.unicode.org/versions/Unicode13.0.0/ch03.pdf) 
- [Stack Overflow : discussion sur les performances de `uppercased()` en Swift](https://stackoverflow.com/questions/28288148/making-my-function-calculate-faster)
