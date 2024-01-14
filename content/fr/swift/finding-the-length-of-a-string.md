---
title:                "Swift: Trouver la longueur d'une chaîne."
simple_title:         "Trouver la longueur d'une chaîne."
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/swift/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Pourquoi

Trouver la longueur d'une chaîne de caractères est une tâche fréquemment réalisée lors de la programmation en Swift. Que ce soit pour vérifier si la saisie d'un utilisateur dépasse une certaine limite ou pour manipuler des données dans une application, connaître la longueur d'une chaîne est essentiel pour de nombreuses opérations.

## Comment Faire

Pour trouver la longueur d'une chaîne en Swift, vous pouvez utiliser la méthode `count` disponible pour les types de données `String`. Cette méthode renvoie le nombre de caractères présents dans une chaîne donnée.

```Swift
let texte = "Bonjour!"

print(texte.count)
// Affiche 8, car la chaîne contient 8 lettres
```

Si vous souhaitez ignorer les espaces blancs et mettre en compter uniquement les caractères significatifs, vous pouvez utiliser la méthode `trimmingCharacters(in:)` pour créer une nouvelle chaîne sans les espaces blancs, puis utiliser `count` pour trouver sa longueur.

```Swift
let texte = "  Bonjour!  "

let texteSansEspaces = texte.trimmingCharacters(in: .whitespacesAndNewlines)
print(texteSansEspaces.count)
// Affiche 8, car la nouvelle chaîne ne contient pas d'espaces blancs
```

## Plongée Profonde

Il est important de savoir que la méthode `count` compte les caractères visibles d'une chaîne, mais ne prend pas en compte les émoticônes, symboles ou caractères spéciaux qui peuvent occuper plus d'un "emplacement" dans une chaîne. Par exemple, l'émoji drapeau français 🇫🇷 est composé de deux caractères, mais `count` le compte comme un seul.

```Swift
let texte = "Bonjour 🇫🇷"

print(texte.count)
// Affiche 11, car le drapeau français compte pour un seul caractère
```

Cela peut provoquer des erreurs si vous comptez uniquement sur `count` pour manipuler des chaînes contenant des émoticônes ou des symboles spéciaux. Il est important de bien comprendre le fonctionnement de cette méthode et d'utiliser des alternatives si nécessaire, comme `utf16.count` pour compter le nombre de "surrogates" (groupes de caractères) dans une chaîne.

## Voir Aussi

- [Documentation Apple sur la méthode `count`](https://developer.apple.com/documentation/swift/string/2427941-count?changes=latest_minor)
- [Guide de la programmation en Swift de la série "Développement iOS" en français](https://developer.apple.com/fr/documentation/swift/)
- [Article sur les valeurs Unicode en Swift](https://www.hackingwithswift.com/example-code/strings/how-to-work-with-unicode-characters-in-swift)