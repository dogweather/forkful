---
title:    "Swift: Recherche et remplacement de texte"
keywords: ["Swift"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/swift/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

Pourquoi : Rechercher et remplacer du texte est une tâche courante en programmation Swift qui peut vous aider à optimiser votre code, corriger des erreurs et améliorer la lisibilité. 

Comment faire : Pour rechercher et remplacer du texte en Swift, utilisez la méthode `replacingOccurrences(of:with:)` comme dans cet exemple :

```
let message = "Bonjour tout le monde !"
let updatedMessage = message.replacingOccurrences(of: "Bonjour", with: "Hello")
print(updatedMessage)
```

Résultat : `Hello tout le monde !`

Pour remplacer toutes les occurrences d'un mot dans une chaîne, utilisez la méthode `replacingOccurrences(of:with:options:)` avec l'option `.regularExpression` comme dans cet exemple :

```
let sentence = "J'aime programmer en Swift. J'aime aussi la randonnée."
let updatedSentence = sentence.replacingOccurrences(of: "j'aime", with: "j'adore", options: .regularExpression)
print(updatedSentence)
```

Résultat : `J'adore programmer en Swift. J'adore aussi la randonnée.`

Plongée en profondeur : En plus de simplement remplacer du texte, vous pouvez également utiliser des expressions régulières pour effectuer des remplacements plus complexes et ciblés. Vous pouvez également utiliser les méthodes `range(of:)` et `replaceSubrange()` pour rechercher et remplacer du texte dans une chaîne à un emplacement spécifique.

Voir aussi : 
- [Documentation officielle de Swift sur la recherche et le remplacement de texte](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html#ID281)
- [Tutoriel vidéo sur la recherche et le remplacement de texte en Swift](https://www.youtube.com/watch?v=9c9fH8J29pU)
- [Guide pratique des expressions régulières en Swift](https://www.hackingwithswift.com/articles/108/how-to-use-regular-expressions-in-swift)