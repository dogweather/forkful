---
title:                "Suppression de caractères correspondant à un modèle"
html_title:           "Swift: Suppression de caractères correspondant à un modèle"
simple_title:         "Suppression de caractères correspondant à un modèle"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/swift/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Pourquoi

Il peut être utile de supprimer des caractères correspondant à un motif dans une chaîne de caractères pour nettoyer ou formater des données, ou pour effectuer des opérations spécifiques sur une chaîne.

## Comment faire

```Swift
let string = "Bonjour les amis !"

// En utilisant la méthode `replacingOccurrences(of:with:)` pour supprimer les espaces
let newString = string.replacingOccurrences(of: " ", with: "")
// Résultat: "Bonjourlesamis!"

// En utilisant une boucle for avec une condition if pour supprimer les chiffres
var newString = ""
for character in string {
    if !character.isNumber {
        newString.append(character)
    }
}
// Résultat: "Bonjour les amis !"

// En utilisant la méthode `filter()` pour supprimer tous les caractères sauf les lettres
let newString = String(string.filter { $0.isLetter })
// Résultat: "Bonjourlesamis"
```

## Plongée en profondeur

Il existe plusieurs façons de supprimer des caractères correspondant à un motif dans une chaîne de caractères en utilisant les méthodes fournies par le langage Swift. La méthode `removeAll(where:)` peut également être utilisée pour supprimer des caractères selon une condition donnée. De plus, il est important de comprendre comment les indices fonctionnent dans les chaînes de caractères afin de ne pas supprimer les caractères incorrects. 

## Voir aussi

- [Documentation officielle Swift - Traitement de chaînes de caractères](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html)
- [Tutoriel video - Comment supprimer des caractères dans une chaîne en Swift](https://www.youtube.com/watch?v=wai-FHBpXKU)
- [Exemples de code pour supprimer des caractères en Swift](https://www.hackingwithswift.com/example-code/strings/how-to-remove-a-prefix-from-a-string)