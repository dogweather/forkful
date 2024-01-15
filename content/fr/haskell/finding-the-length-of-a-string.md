---
title:                "Trouver la longueur d'une chaîne de caractères"
html_title:           "Haskell: Trouver la longueur d'une chaîne de caractères"
simple_title:         "Trouver la longueur d'une chaîne de caractères"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/haskell/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

# Pourquoi

Vous avez probablement été confronté à la tâche de trouver la longueur d'une chaîne de caractères lors de vos projets de programmation. Cela peut sembler un problème simple, mais il est important de comprendre comment le faire correctement en utilisant Haskell.

# Comment faire

Pour trouver la longueur d'une chaîne de caractères en Haskell, nous pouvons utiliser la fonction `length`. Voyons un exemple:

```Haskell
length "Bonjour" -- renvoie 7
```

Nous pouvons également utiliser `length` sur une liste de chaînes de caractères pour trouver la longueur totale de la liste:

```Haskell
length ["Bonjour", "mon", "ami"] -- renvoie 13
```

Nous pouvons également définir notre propre fonction pour trouver la longueur d'une chaîne de caractères en utilisant la récursivité:

```Haskell
length' :: String -> Int
length' [] = 0 -- cas de base, une chaîne vide a une longueur de 0
length' (x:xs) = 1 + length' xs -- pour chaque élément de la chaîne, ajoute 1 à la longueur totale

length' "Bonjour" -- renvoie 7
```

# Plongée en profondeur

Il est important de noter que la fonction `length` renvoie un entier de type `Int` et non un entier de type `Integer`. Cela signifie que si la longueur de la chaîne de caractères dépasse la limite d'un entier, le résultat sera incorrect ou il y aura une erreur de débordement de mémoire.

De plus, la fonction `length` fonctionne en parcourant entièrement la chaîne de caractères, ce qui peut prendre du temps si la chaîne est très longue. Dans ces cas-là, il peut être plus efficace d'utiliser la fonction `length'` définie précédemment.

# Voir aussi

- [Documentation sur la fonction `length`](https://www.haskell.org/onlinereport/standard-prelude.html#function:length)
- [Un tutoriel sur les listes en Haskell](https://www.tutorialspoint.com/haskell/haskell_lists.htm)