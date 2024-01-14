---
title:                "Swift: Suppression de caractères correspondant à un motif"
programming_language: "Swift"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/swift/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Pourquoi

Supprimer des caractères correspondant à un motif est un moyen efficace de nettoyer et de manipuler des données dans un programme Swift. Que vous ayez besoin d'enlever des espaces, des signes de ponctuation ou d'autres caractères spéciaux, cette technique peut vous aider à simplifier votre code et à obtenir les résultats souhaités.

## Comment faire

Voici un exemple de code Swift qui utilise la méthode `replacingOccurrences` pour supprimer tous les espaces dans une chaîne de caractères.

```Swift
let phrase = "Bonjour, le monde!"
let nouvellePhrase = phrase.replacingOccurrences(of: " ", with: "")
print(nouvellePhrase)
```

Ce code produira une sortie de `Bonjour,lemonde!`. Vous pouvez remplacer l'espace avec n'importe quel autre caractère ou chaîne de caractères, en fonction de vos besoins spécifiques.

## Plongeons plus en profondeur

Il y a plusieurs façons de supprimer des caractères correspondant à un motif dans Swift, en utilisant des méthodes telles que `range(of:)` et `filter`. Vous pouvez également utiliser les expressions régulières pour une manipulation plus avancée de la chaîne de caractères. Assurez-vous de consulter la documentation officielle Swift pour en savoir plus sur ces méthodes et d'autres techniques utiles.

## Voir aussi

- [Documentation officielle Swift](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html)
- [Supprimer un caractère d'une chaîne de caractères en Swift](https://www.hackingwithswift.com/example-code/strings/how-to-remove-a-character-from-a-string-using-index)
- [Manipulation de chaînes de caractères avec des expressions régulières en Swift](https://www.raywenderlich.com/821780-regular-expressions-tutorial-for-swift-getting-started)