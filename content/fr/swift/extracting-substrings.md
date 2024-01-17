---
title:                "Extraction de sous-chaînes"
html_title:           "Swift: Extraction de sous-chaînes"
simple_title:         "Extraction de sous-chaînes"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/swift/extracting-substrings.md"
---

{{< edit_this_page >}}

## Quoi & pourquoi?

L'extraction de sous-chaînes est une méthode utilisée en programmation pour récupérer une partie spécifique d'une chaîne de caractères. Les programmeurs utilisent cette technique pour manipuler, filtrer, ou formater des données en fonction de leurs besoins.

## Comment faire?

Utiliser la méthode de sous-chaîne en Swift est très simple. Il suffit d'utiliser la fonction `substring` sur une chaîne de caractères et de spécifier l'indice de début et de fin de la sous-chaîne que vous souhaitez extraire. Voici un exemple:

```
let fullString = "Bonjour tout le monde"
let substring = fullString.substring(from: 8, to: 11)
print(substring) // affiche: "tout"
```

## Plongez plus profondément

L'extraction de sous-chaînes existe depuis longtemps dans le monde de la programmation. Auparavant, les programmeurs utilisaient des fonctions telles que `substr` en C ou `substring` en Java pour accomplir cette tâche. Dans Swift, il est recommandé d'utiliser la méthode `substring`.

Il existe également d'autres méthodes pour extraire des sous-chaînes, telles que `prefix` et `suffix` qui permettent d'extraire respectivement le début ou la fin d'une chaîne de caractères.

L'implémentation de la méthode de sous-chaîne en Swift utilise une structure de données appelée `String.Index`, qui permet de manipuler les indices des caractères dans une chaîne. Cela garantit que l'extraction de sous-chaînes est efficace et précise.

## Voir aussi

Pour en savoir plus sur l'extraction de sous-chaînes en Swift, vous pouvez consulter la documentation officielle [ici](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html#ID364). Vous pouvez également explorer les différentes méthodes et fonctions disponibles pour manipuler les chaînes de caractères en Swift sur le [site d'Apple](https://developer.apple.com/documentation/swift/string) (en anglais).