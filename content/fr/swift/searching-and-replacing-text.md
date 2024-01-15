---
title:                "Recherche et remplacement de texte"
html_title:           "Swift: Recherche et remplacement de texte"
simple_title:         "Recherche et remplacement de texte"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/swift/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Pourquoi

Voici pourquoi vous devriez vous intéresser à la recherche et au remplacement de texte en Swift : cela vous permet de rapidement et efficacement faire des modifications dans de grands volumes de texte sans avoir à les faire manuellement.

## Comment faire

Voici comment utiliser la fonction intégrée de recherche et de remplacement de texte en Swift :

```Swift
let text = "Bonjour tout le monde ! Bienvenue sur mon blog de développement Swift."

let modifiedText = text.replacingOccurrences(of: "Swift", with: "programmation", options: .caseInsensitive, range: nil)

print(modifiedText)
```

Le code ci-dessus remplace le mot "Swift" par "programmation" dans la chaîne de texte donnée, en ignorant la casse des lettres. L'option "range" vous permet également de limiter la recherche à une partie spécifique de la chaîne de texte, si nécessaire.

La sortie du code ci-dessus sera :

```
Bonjour tout le monde ! Bienvenue sur mon blog de développement programmation.
```

## Plongée en profondeur

En plus de la fonction `replacingOccurrences`, Swift offre d'autres méthodes pour rechercher et remplacer du texte, telles que `replacingCharacters`, `replacingPrefix` et `replacingSuffix`. Ces méthodes vous permettent de cibler des motifs spécifiques dans un texte et de les remplacer par un autre.

Il est également possible d'utiliser des expressions régulières pour une recherche et un remplacement plus avancés de texte en Swift. Les expressions régulières vous permettent de définir des motifs complexes pour la recherche de texte, ce qui peut être utile dans certains cas.

## Voir aussi

- [Documentation officielle Apple sur la recherche et le remplacement de texte en Swift](https://developer.apple.com/documentation/swift/string/2941913-replacingoccurrences)
- [Tutorial sur l'utilisation des expressions régulières en Swift](https://www.raywenderlich.com/86205/regular-expressions-swift)