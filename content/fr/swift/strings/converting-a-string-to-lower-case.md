---
date: 2024-01-20 17:39:16.631127-07:00
description: "How to: Swift rend la conversion en minuscules \xE9tonnamment simple.\
  \ Voici comment ."
lastmod: '2024-03-13T22:44:58.201641-06:00'
model: gpt-4-1106-preview
summary: "Swift rend la conversion en minuscules \xE9tonnamment simple."
title: "Conversion d'une cha\xEEne de caract\xE8res en minuscules"
weight: 4
---

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
