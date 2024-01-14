---
title:    "Swift: Suppression des caractères correspondant à un motif"
keywords: ["Swift"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/swift/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Pourquoi

Souvent, lors de la programmation en Swift, vous pouvez rencontrer une situation où vous devez supprimer certains caractères d'une chaîne de caractères en fonction d'un modèle spécifique. Cela peut sembler fastidieux à première vue, mais il existe en fait une méthode simple pour le faire.

## Comment Faire

Pour supprimer des caractères correspondant à un modèle en Swift, vous devez utiliser la méthode `replacingOccurrences()` de la classe `NSString`. Cette méthode prend en paramètre le modèle que vous souhaitez supprimer et le caractère par lequel vous souhaitez le remplacer. Voici un exemple de code:

```Swift
let myString = "Bonjour tout le monde"
let newString = myString.replacingOccurrences(of: "o", with: "")
print(newString)
```

Cet exemple va imprimer "Bnjur tut le mnde", car tous les "o" ont été supprimés de la chaîne originale.

## Plongée Profonde

Il est important de noter que la méthode `replacingOccurrences()` est sensible à la casse, ce qui signifie qu'elle ne remplacera que les caractères correspondant exactement au modèle spécifié. De plus, elle ne supprimera pas les caractères spéciaux ou les espaces.

Si vous souhaitez supprimer les caractères correspondant à un modèle indépendamment de la casse, vous pouvez utiliser la méthode `replacingOccurrencesWithCaseInsensitive()` à la place.

Vous pouvez également utiliser des expressions régulières dans la méthode `replacingOccurrences()` pour des modifications plus complexes. Cependant, cela nécessite une compréhension plus approfondie des expressions régulières et peut s'avérer plus complexe.

## Voir Aussi

- [Documentation Apple sur la méthode `replacingOccurrences()`](https://developer.apple.com/documentation/foundation/nsstring/1408728-replacingoccurrences)
- [Guide complet sur les expressions régulières en Swift](https://www.swift-studies.com/blog/2018/8/7/regular-expressions)