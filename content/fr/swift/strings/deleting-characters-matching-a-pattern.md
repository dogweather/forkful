---
title:                "Suppression de caractères correspondant à un motif"
aliases:
- /fr/swift/deleting-characters-matching-a-pattern.md
date:                  2024-01-20T17:43:19.399530-07:00
model:                 gpt-4-1106-preview
simple_title:         "Suppression de caractères correspondant à un motif"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/swift/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## What & Why?
(Suppression de caractères correspondant à un motif)

Dans la programmation, lorsque nous parlons de supprimer des caractères selon un motif, on fait référence à l'élimination sélective de certains caractères dans une chaîne de texte qui correspondent à un modèle spécifique. Les programmeurs font cela pour nettoyer les données, par exemple, en supprimant des balises HTML d'une chaîne ou en assurant qu'un numéro de téléphone respecte un format particulier.

## How to:
(Comment faire : )

En Swift, on peut utiliser les expressions régulières (regex) pour identifier et supprimer les caractères correspondant à un motif. Voici comment on procède :

```swift
import Foundation

func deleteMatchingCharacters(from string: String, pattern: String) -> String {
    let regex = try! NSRegularExpression(pattern: pattern, options: [])
    let range = NSRange(string.startIndex..., in: string)
    return regex.stringByReplacingMatches(in: string, options: [], range: range, withTemplate: "")
}

// Exemple : Supprimer les chiffres
let originalString = "Appeler au 123-456-7890."
let pattern = "[0-9]"
let cleanString = deleteMatchingCharacters(from: originalString, pattern: pattern)

print(cleanString) // Affiche: "Appeler au -.-"
```
Notez que l'exemple ci-dessus utilise la classe NSRegularExpression, qui fait partie du framework Foundation. Dans ce contexte, "[0-9]" est le modèle (ou pattern) représentant tous les chiffres, et on les remplace par rien, ce qui les supprime.

## Deep Dive:
(Plongée en profondeur)

La suppression de caractères par motif existe depuis longtemps en programmation; elle devient cruciale pour le traitement de texte et la manipulation de données. En Swift, `NSRegularExpression` est une classe inspirée de la bibliothèque ICU qui est utilisée largement à travers différents langages pour les opérations avec les expressions régulières.

En dehors de `NSRegularExpression`, les développeurs Swift peuvent aussi utiliser des méthodes de String comme `replacingOccurrences(of:with:)` pour des remplacements simples, qui ne nécessitent pas des motifs complexes.

Le détail crucial de l'implémentation avec les regex en Swift, c'est la gestion des erreurs. Dans l'exemple donné, on utilise `try!` pour dire que l'expression régulière est correcte et ne causera pas d'erreur. Cependant, dans une application réelle, il est préférable d'utiliser `try?` ou `do-catch` pour gérer les erreurs potentielles.

## See Also:
(Voir aussi)

- Documentation Swift sur les expressions régulières : [NSRegularExpression](https://developer.apple.com/documentation/foundation/nsregularexpression)
- Tutorial Swift sur le traitement de textes : [Ray Wenderlich's Text Processing](https://www.raywenderlich.com/116-an-nsregularexpression-tutorial-in-swift)
- ICU Library, d'où NSRegularExpression tire ses origines : [ICU User Guide](http://userguide.icu-project.org/strings/regexp)
