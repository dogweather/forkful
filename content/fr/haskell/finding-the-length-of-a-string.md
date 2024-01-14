---
title:    "Haskell: Trouver la longueur d'une chaîne de caractères"
keywords: ["Haskell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/haskell/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Pourquoi

La recherche de la longueur d'une chaîne de caractères est une tâche courante en programmation, cela permet de mieux comprendre le fonctionnement des chaînes et de les manipuler en conséquence.

## Comment faire

Pour trouver la longueur d'une chaîne de caractères en Haskell, il existe plusieurs méthodes :

```Haskell
-- Méthode 1 : Utilisation de la fonction prédéfinie "length"
length "Bonjour" -- Sortie attendue : 7

-- Méthode 2 : Utilisation de la récursivité
len :: String -> Int -- Déclaration de la fonction
len [] = 0 -- Cas de base : la chaîne est vide, donc la longueur est 0
len (x:xs) = 1 + len xs -- Pour chaque caractère, on ajoute 1 à la longueur et on passe au caractère suivant
len "Bonjour" -- Sortie attendue : 7
```

## Plongée en profondeur

La fonction préférée de la plupart des développeurs Haskell pour trouver la longueur d'une chaîne de caractères est la récursivité. Cela permet d'avoir une meilleure compréhension de la structure de la chaîne et du fonctionnement de la récursivité en Haskell. Cela peut également être utile pour effectuer des manipulations plus complexes sur les chaînes de caractères. Cependant, si vous recherchez juste une solution rapide et facile, la fonction prédéfinie "length" est votre meilleur choix.

## Voir aussi

- Documentation sur la fonction "length" : [https://hackage.haskell.org/package/base/docs/Prelude.html#v:length](https://hackage.haskell.org/package/base/docs/Prelude.html#v:length)
- Tutoriel sur la récursivité en Haskell : [https://www.tutorialspoint.com/haskell/haskell_recursion.htm](https://www.tutorialspoint.com/haskell/haskell_recursion.htm)
- Autres fonctions utiles pour les manipulations de chaînes de caractères : [https://wiki.haskell.org/Strings](https://wiki.haskell.org/Strings)