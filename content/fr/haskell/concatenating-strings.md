---
title:                "Haskell: Concaténation de chaînes de caractères"
programming_language: "Haskell"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/haskell/concatenating-strings.md"
---

{{< edit_this_page >}}

## Pourquoi
Si vous êtes nouveau dans le monde de la programmation Haskell, vous vous demandez peut-être pourquoi quelqu'un voudrait concaténer des chaînes de caractères. En termes simples, la concaténation de chaînes de caractères implique la fusion de deux ou plusieurs chaînes de caractères pour en créer une seule.

## Comment faire
La concaténation de chaînes de caractères en Haskell est simple et facile. Avec l'opérateur "++", vous pouvez concaténer deux chaînes de caractères comme dans l'exemple suivant :

```Haskell
"Bonjour" ++ "à tous"
```
Cela produirait la sortie suivante : "Bonjour à tous".

Vous pouvez également concaténer plusieurs chaînes de caractères en les mettant toutes ensemble, comme ceci :

```Haskell
"Comment" ++ "" ++ "ça" ++ "va ?"
```
Cela produirait la sortie suivante : "Comment ça va ?".

## Plongée en profondeur
En plus de l'opérateur "++", il existe également la fonction "concat" qui concatène une liste de chaînes de caractères en une seule. Vous pouvez également utiliser des variables pour stocker des chaînes de caractères et les concaténer à l'aide de l'opérateur "++=".

Il est important de noter que la concaténation de chaînes de caractères peut être coûteuse en termes de performances, car elle nécessite l'allocation d'une nouvelle chaîne de caractères à chaque concaténation. Il est donc recommandé d'utiliser la fonction "concat" ou "lines" lorsque vous avez besoin de concaténer plusieurs chaînes de caractères.

## Voir aussi
- [Documentation officielle sur la concaténation de chaînes de caractères en Haskell](https://www.haskell.org/documentation/#strings)
- [Tutoriel sur la concaténation de chaînes de caractères en Haskell](https://www.tutorialspoint.com/haskell/haskell_concatenation.htm)
- [Vidéo sur la concaténation de chaînes de caractères en Haskell](https://www.youtube.com/watch?v=uFxkRcMjAyM)