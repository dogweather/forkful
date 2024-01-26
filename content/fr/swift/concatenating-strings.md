---
title:                "Concaténation de chaînes de caractères"
date:                  2024-01-20T17:35:45.672979-07:00
model:                 gpt-4-1106-preview
simple_title:         "Concaténation de chaînes de caractères"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/swift/concatenating-strings.md"
---

{{< edit_this_page >}}

## What & Why? (Quoi et Pourquoi ?)
Concaténer des chaînes, c'est les joindre bout à bout. On fait ça pour créer des phrases, des messages d'erreur, ou assembler des bouts de texte de différentes sources.

## How to: (Comment faire : )
```Swift
// Utiliser l'opérateur '+'
let salut = "Salut, "
let nom = "Jean!"
let message = salut + nom
print(message) // "Salut, Jean!"

// Concaténation avec '\(variable)'
let age = 25
let phrase = "J'ai \(age) ans."
print(phrase) // "J'ai 25 ans."

// Utiliser la méthode 'append()'
var invitation = "Bienvenue "
let invite = "Marie"
invitation.append(invite)
print(invitation) // "Bienvenue Marie"
```

## Deep Dive: (Plongée en profondeur)
Historiquement, concaténer des chaînes de caractères était une affaire de pointeurs et de manipulation de mémoire, spécialement en langages comme C. En Swift, la simplicité prime : l'opérateur `+` et l'interpolation de chaîne `\(variable)` rendent le tout intuitif. Les alternatives comme la méthode `append()` ou `joining(separator:)` sont là pour des cas spéciaux. Par exemple, `append()` est pratique pour ajouter sur place sans créer une nouvelle chaîne. Niveau performance, pour une utilisation intensive, prenez garde à la concaténation dans des boucles : choisissez des stratégies comme `String` ou `Array` buffer qui sont optimisés pour cela.

## See Also: (Voir aussi)
- La documentation Swift sur les chaînes de caractères : [Documentation Apple](https://developer.apple.com/documentation/swift/string)
- Plus d'astuces sur l'optimisation de la concaténation de chaînes en Swift sur Stack Overflow : [Stack Overflow Swift String Concatenation](https://stackoverflow.com/questions/tagged/swift+string-concatenation)
