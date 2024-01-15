---
title:                "Majusculation d'une chaîne de caractères"
html_title:           "Haskell: Majusculation d'une chaîne de caractères"
simple_title:         "Majusculation d'une chaîne de caractères"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/haskell/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Pourquoi

Si vous avez déjà travaillé avec des chaînes de caractères en Haskell, vous savez qu'il existe plusieurs fonctions pour les transformer, mais aucune pour les mettre en majuscules. Heureusement, il est très simple de capitaliser une chaîne de caractères en Haskell, et dans cet article, je vais vous montrer comment le faire.

## Comment faire

Pour capitaliser une chaîne de caractères en Haskell, nous allons utiliser la fonction `toUpper` de la bibliothèque `Data.Char`. Pour ce faire, commençons par importer cette bibliothèque :

```Haskell
import Data.Char
```

Ensuite, nous allons définir notre fonction `capitalize`, qui prendra en entrée une chaîne de caractères et renverra la même chaîne, mais en majuscules :

```Haskell
-- Définition de la fonction capitalize
capitalize :: String -> String
capitalize str = map toUpper str
```

Maintenant, essayons notre fonction avec une chaîne de caractères :

```Haskell
capitalize "bonjour"  -- Renvoie "BONJOUR"
```

Et voilà, c'est aussi simple que ça ! Notre fonction fonctionne avec toutes sortes de chaînes de caractères, qu'elles soient en minuscules, en majuscules, avec des caractères spéciaux, etc.

## Deep Dive

Maintenant que nous avons vu comment capitaliser une chaîne de caractères en Haskell, parlons un peu plus en profondeur de la fonction `toUpper`. Cette fonction fait partie du type `Char -> Char`, ce qui signifie qu'elle prend un caractère en entrée et renvoie un caractère en sortie. Elle utilise la table ASCII pour convertir un caractère en minuscule en son équivalent en majuscule.

Il est également intéressant de noter que la fonction `toUpper` ne fait pas de distinction entre les caractères accentués et non accentués. Par exemple, si nous passons le caractère "é" à la fonction, elle renverra le caractère "É", le même résultat que si nous avions donné "E" en entrée.

## Voir aussi

- [Documentation de la bibliothèque `Data.Char`](https://hackage.haskell.org/package/base/docs/Data-Char.html)
- [Tutoriel sur les chaînes de caractères en Haskell](https://wiki.haskell.org/String)