---
title:                "Haskell: Majuscule d'une chaîne de caractères"
programming_language: "Haskell"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/haskell/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Pourquoi

Capitaliser une chaîne de caractères peut sembler être une tâche simple et banale, mais c'est un concept important à comprendre pour tout programmeur Haskell. Dans cet article, nous allons expliquer pourquoi il est important de savoir comment capitaliser une chaîne de caractères et comment le faire efficacement en utilisant Haskell.

## Comment faire

Pour capitaliser une chaîne de caractères en Haskell, nous pouvons utiliser la fonction `toUpper` du module `Data.Char`. Cette fonction prend un caractère en entrée et renvoie le caractère correspondant en majuscule. Pour capitaliser toute une chaîne de caractères, nous pouvons utiliser la fonction `map` en passant la fonction `toUpper` en argument.

```Haskell
import Data.Char

-- Fonction pour capitaliser une chaîne de caractères
capitalize :: String -> String
capitalize str = map toUpper str

-- Exemple d'utilisation
capitalize "haskell" -- "HASKELL"
capitalize "programmation fonctionnelle" -- "PROGRAMMATION FONCTIONNELLE"
```

Voyons maintenant un exemple plus complexe où nous voulons capitaliser uniquement le premier mot d'une phrase :

```Haskell
-- Fonction pour capitaliser seulement la première lettre d'une chaîne de caractères
capitalizeFirst :: String -> String
capitalizeFirst [] = []
capitalizeFirst (x:xs) = (toUpper x) : xs

-- Fonction pour capitaliser le premier mot d'une phrase
capitalizePhrase :: String -> String
capitalizePhrase [] = []
capitalizePhrase str = unwords (map capitalizeFirst (words str))

-- Exemple d'utilisation
capitalizePhrase "je suis un programmeur Haskell" -- "Je suis un programmeur Haskell"
capitalizePhrase "apprendre à programmer en Haskell" -- "Apprendre à programmer en Haskell"
```

Nous pouvons également utiliser la fonction `interact` pour capitaliser une chaîne de caractères saisie par l'utilisateur :

```Haskell
-- Fonction pour capitaliser une chaîne de caractères saisie par l'utilisateur
capitalizeInput :: String -> String
capitalizeInput str = capitali