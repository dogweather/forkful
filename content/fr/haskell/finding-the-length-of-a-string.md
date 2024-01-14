---
title:                "Haskell: Trouver la longueur d'une chaîne de caractères"
programming_language: "Haskell"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/haskell/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

# Pourquoi

Si vous êtes débutant en programmation Haskell, vous pouvez vous demander pourquoi il est important de savoir comment trouver la longueur d'une chaîne de caractères. En réalité, cette compétence est essentielle car il s'agit d'une tâche fréquemment utilisée dans la manipulation de données.

# Comment faire

Pour trouver la longueur d'une chaîne de caractères en Haskell, il existe une fonction simple appelée `length`. Voyons un exemple :

```Haskell
-- Déclaration d'une chaîne de caractères
let chaine = "Bonjour!"

-- Utilisation de la fonction length pour trouver la longueur de la chaîne de caractères
length chaine
```

Résultat :

```
8
```

Comme vous pouvez le voir, la fonction `length` renvoie la longueur de la chaîne en comptant tous les caractères, y compris les espaces.

Il est également possible d'utiliser cette fonction avec une liste de chaînes de caractères. Dans ce cas, la longueur de chaque chaîne sera renvoyée sous forme de liste. Voyons un autre exemple :

```Haskell
-- Déclaration d'une liste de chaînes de caractères
let liste = ["Bonjour", "tout", "le", "monde!"]

-- Utilisation de la fonction length pour trouver la longueur de chaque chaîne dans la liste
length liste
```

Résultat :

```
[7,4,2,7]
```

# Plongée en profondeur

La fonction `length` est en fait une fonction récursive basée sur le principe de compter le nombre d'éléments dans une liste. Elle peut être définie comme suit :

```Haskell
length :: [a] -> Int
length [] = 0                                 -- si la liste est vide, sa longueur est de 0
length (x:xs) = 1 + length xs                 -- pour chaque élément dans la liste, on ajoute 1 à la longueur
```

Il est également important de noter que la fonction `length` ne fonctionne que sur des chaînes de caractères ou des listes de même type. Par exemple, si une liste contient à la fois des chaînes de caractères et des nombres, la fonction ne pourra pas être appliquée.

# Voir aussi

- [La documentation officielle de la fonction `length`](https://hackage.haskell.org/package/base-4.12.0.0/docs/Data-List.html#v:length)
- [Un tutoriel sur les fonctions récursives en Haskell](https://www.tutorialspoint.com/haskell/haskell_functions.htm)