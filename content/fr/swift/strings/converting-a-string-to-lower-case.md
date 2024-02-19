---
aliases:
- /fr/swift/converting-a-string-to-lower-case/
date: 2024-01-20 17:39:16.631127-07:00
description: "Convertir une cha\xEEne de caract\xE8res en minuscules, c'est transformer\
  \ tous les caract\xE8res alphab\xE9tiques en leur version minuscule. On le fait\
  \ pour\u2026"
lastmod: 2024-02-18 23:09:09.194502
model: gpt-4-1106-preview
summary: "Convertir une cha\xEEne de caract\xE8res en minuscules, c'est transformer\
  \ tous les caract\xE8res alphab\xE9tiques en leur version minuscule. On le fait\
  \ pour\u2026"
title: "Conversion d'une cha\xEEne de caract\xE8res en minuscules"
---

{{< edit_this_page >}}

## What & Why?
Convertir une chaîne de caractères en minuscules, c'est transformer tous les caractères alphabétiques en leur version minuscule. On le fait pour uniformiser les données, simplifier les comparaisons de textes et souvent lors de la gestion des entrées utilisateurs.

## How to:
Swift rend la conversion en minuscules étonnamment simple. Voici comment :

```Swift
let phrase = "Bonjour, Programmeur!"
let phraseEnMinuscules = phrase.lowercased()

print(phraseEnMinuscules)
```

Sortie :
```
bonjour, programmeur!
```

Facile, n'est-ce pas ?

## Deep Dive
Historiquement, la normalisation de chaînes en minuscules est un héritage de l'époque où les données étaient comparées manuellement – pensons aux grands annuaires ou aux listings informatiques. En Swift, la méthode `lowercased()` est une partie de la richesse de `String`. Bien qu'elle soit souvent suffisante, il existe des alternatives pour les contextes plus complexes, notamment avec `Locale`. Par exemple, en turc, les règles de casse sont différentes, ce qui nécessite une localisation pour une conversion correcte. `String` en Swift est conforme à `Unicode`, ce qui signifie que toutes les transformations de casse sont faites avec les meilleurs pratiques internationales en tête.

## See Also
Pour explorer davantage, jetez un œil à ces ressources :

- [Unicode](http://unicode.org/) pour comprendre comment les caractères sont représentés dans différentes langues.
- La [classe `Locale`](https://developer.apple.com/documentation/foundation/locale) de Swift, utile pour les opérations qui dépendent de la langue et de la région.

Voilà ! Vous avez maintenant une bonne base pour travailler avec les chaînes en minuscules dans Swift. Bonne programmation !
