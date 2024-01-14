---
title:                "Haskell: Recherche et remplacement de texte"
programming_language: "Haskell"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/haskell/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Pourquoi
Lorsque vous programmez en Haskell, vous pourriez avoir à effectuer des recherches et des remplacements de texte dans votre code. Cela peut sembler fastidieux, mais heureusement, Haskell a des fonctions intégrées pour vous aider à le faire efficacement. Découvrez comment effectuer des recherches et des remplacements de texte en Haskell!

## Comment faire
La fonction de base pour effectuer des recherches et des remplacements de texte en Haskell est `replace`. Elle prend en entrée un motif de recherche, une chaîne de remplacement et un texte dans lequel effectuer la recherche. Voici un exemple simple:

```Haskell
replace "chat" "chien" "J'aime les chats"
```

Cela renverra la chaîne de texte "J'aime les chiens". Notez que cela ne modifie pas directement la chaîne de texte originale, cela renvoie plutôt une nouvelle chaîne avec les modifications. 

On peut également utiliser la fonction `replace` sur des listes de caractères en utilisant le type `String` en Haskell. Voici un exemple:

```Haskell
replace "H" "Hello" "wrd"
```

Cela renverra "HelloHelloello". Vous pouvez également utiliser la fonction de recherche et de remplacement de manière récursive pour effectuer des modifications complexes dans une chaîne de caractères.

## Plongée en profondeur
En plus de la fonction de base `replace`, Haskell offre également d'autres fonctions utiles pour la recherche et le remplacement de texte. Par exemple, `nub` peut être utilisé pour supprimer tous les doublons dans une liste, tandis que `unfold` peut être utilisé pour générer une liste à partir d'une fonction récursive.

De plus, vous pouvez également utiliser des expressions régulières pour effectuer des recherches et des remplacements de texte en Haskell en important le module `Text.Regex`. Les expressions régulières sont un outil puissant pour la manipulation de chaînes de caractères et peuvent être utilisées pour des recherches plus complexes.

Vous pouvez également utiliser des fonctions de manipulation de chaînes de caractères telles que `take` et `drop` pour extraire des parties spécifiques d'une chaîne de texte avant de la modifier avec la fonction `replace`.

## Voir aussi
- [Documentation sur les fonctions de manipulation de chaînes de caractères en Haskell](http://hackage.haskell.org/package/base-4.14.0.0/docs/Data-List.html#g:23)
- [Tutoriel sur l'utilisation d'expressions régulières en Haskell](https://www.tutorialspoint.com/haskell/haskell_regular_expressions.htm)
- [Exemples pratiques d'utilisation de fonctions de manipulation de chaînes en Haskell](https://blog.infinitenegativeutility.com/2014/5/haskell-lists-strings-and-matching)