---
title:                "Concaténation de chaînes"
html_title:           "Gleam: Concaténation de chaînes"
simple_title:         "Concaténation de chaînes"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/gleam/concatenating-strings.md"
---

{{< edit_this_page >}}

## Pourquoi

Concaténer des chaînes est une tâche courante en programmation qui consiste à fusionner plusieurs chaînes de caractères en une seule. Cela est utile pour construire des phrases ou des messages dynamiques à partir de différentes variables.

## Comment le faire

```Gleam
let prenom = "Marie"
let nom = "Dupont"
let message = "Bonjour, je m'appelle " ++ prenom ++ " " ++ nom ++ "."
```

Ce code va concaténer les variables `prenom` et `nom` avec du texte statique pour créer le message suivant : "Bonjour, je m'appelle Marie Dupont.". Il est également possible de concaténer plusieurs chaînes à la fois en utilisant la fonction `concat` :

```Gleam
let mots = ["Bonjour", "je", "suis", "Gleam"]
let phrase = concat(mots)
// phrase == "Bonjourje' m'appelleGleam"
```

De manière générale, il est important de faire attention à l'ordre de concaténation pour éviter des espaces ou des caractères indésirables dans le résultat final. Pour ajouter des espaces entre les chaînes, il suffit de les inclure dans la concaténation :

```Gleam
let prenom = "Marie"
let nom = "Dupont"
let message = "Bonjour, je m'appelle " ++ prenom ++ " " ++ nom ++ "."
```

## Plongée en profondeur

Il est important de noter que la concaténation de chaînes peut être gourmande en ressources si elle est utilisée à grande échelle. En effet, à chaque concaténation, une nouvelle chaîne est créée en mémoire, ce qui peut entraîner des problèmes de performance. Pour éviter cela, il est recommandé d'utiliser des types spéciaux tels que `StringBuilder` qui permettent de concaténer des chaînes de manière plus efficace.

## Voir aussi

- [Documentation Gleam sur les chaînes](https://gleam.run/documentation/stdlib/string#concat)
- [Article sur les performances des opérations de chaînes en Java](https://dzone.com/articles/string-concatenation-performance-io)
- [Exemples pratiques de concaténation de chaînes en C#](https://www.tutorialsteacher.com/csharp/csharp-string-tutorial)