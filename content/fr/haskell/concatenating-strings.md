---
title:                "Concaténer des chaînes de caractères"
html_title:           "Haskell: Concaténer des chaînes de caractères"
simple_title:         "Concaténer des chaînes de caractères"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/haskell/concatenating-strings.md"
---

{{< edit_this_page >}}

## Pourquoi

Pourquoi est-il utile de concaténer des chaînes de caractères en Haskell ? Et bien, la concaténation de chaînes de caractères est un moyen simple et efficace de combiner plusieurs chaînes en une seule. Cela permet de créer de nouveaux messages à partir de données existantes, ce qui peut être très pratique dans de nombreuses situations en programmation.

## Comment Faire

```Haskell
-- Déclaration d'une fonction concaténer qui prend deux chaînes de caractères en entrée et renvoie leur concaténation
concatener :: String -> String -> String
concatener str1 str2 = str1 ++ str2

-- Utilisation de la fonction avec deux chaînes prédéfinies
concatener "Bonjour" "Haskell" -- Sortie : "Bonjour Haskell"

-- Utilisation de la fonction avec des chaînes saisies par l'utilisateur
putStrLn "Veuillez saisir une première chaîne : "
str1 <- getLine
putStrLn "Veuillez saisir une deuxième chaîne : "
str2 <- getLine
concatener str1 str2 -- Sortie : "Chaîne 1Chaîne 2"
```

La concaténation de chaînes de caractères est réalisée à l'aide de l'opérateur `++` en Haskell. Ce dernier prend en entrée deux chaînes de caractères et renvoie leur concaténation. Il est également possible de concaténer des chaînes saisies par l'utilisateur, comme dans l'exemple ci-dessus.

## Approfondissement

En Haskell, les chaînes de caractères sont représentées par le type `String`, qui n'est en fait qu'un alias pour une liste de caractères (`[Char]`). Ainsi, la concaténation de chaînes de caractères peut être vue comme la concaténation de deux listes de caractères. 

Il est également important de noter que la concaténation de chaînes de caractères peut se faire avec plus de deux chaînes, en les combinant les unes après les autres. Par exemple :

```Haskell
concatener "Bonjour" "Haskell" "!" -- Sortie : "Bonjour Haskell!"
```

Enfin, il est possible de concaténer des chaînes de caractères avec d'autres types de données en Haskell, à condition que ces derniers puissent être convertis en chaînes. Cette conversion peut être réalisée grâce à la fonction `show`.

## Voir Aussi

- [Documentation officielle de concaténation de chaînes en Haskell](https://www.haskell.org/tutorial/strings.html#concatenation) 
- [Tutoriel sur la manipulation de chaînes de caractères en Haskell](https://www.tutorialspoint.com/haskell/haskell_strings.htm) 
- [Exemples de concaténation de chaînes en Haskell](https://wiki.haskell.org/How_to_work_on_lists)