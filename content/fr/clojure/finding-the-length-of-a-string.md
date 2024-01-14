---
title:                "Clojure: Trouver la longueur d'une chaîne"
programming_language: "Clojure"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/clojure/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Pourquoi
Trouver la longueur d'une chaîne de caractères est une tâche courante en programmation, peu importe le langage que vous utilisez. Cela peut être utile pour vérifier si une chaîne est assez longue pour être stockée dans une base de données ou pour effectuer des opérations de manipulation de chaînes.

## Comment faire

Pour trouver la longueur d'une chaîne en Clojure, vous pouvez utiliser la fonction `count`. Par exemple:

```
Clojure (count "Bonjour")
=> 7
```

Cette fonction renverra le nombre de caractères dans la chaîne donnée. Vous pouvez également l'utiliser sur d'autres types de données, comme des listes ou des vecteurs.

```
Clojure (count [1 2 3 4])
=> 4
```

Si vous souhaitez trouver la longueur d'une chaîne en utilisant des boucles ou des récursions, il existe plusieurs façons de le faire. Vous pouvez parcourir chaque caractère de la chaîne en utilisant `nth` et `count`, ou utiliser une boucle `for`.

## Plongée en profondeur

Alors que la fonction `count` est un moyen simple et rapide de trouver la longueur d'une chaîne, il est important de comprendre comment elle fonctionne réellement. En réalité, elle utilise une approche récursive en interne pour parcourir la chaîne et compter chaque caractère jusqu'à ce qu'elle atteigne la fin.

Une chose intéressante à noter est que `count` utilisera une approche différente en fonction du type de données fourni. Pour les chaînes, elle utilisera la méthode `length` de la classe Java String, tandis que pour d'autres types de données, elle utilisera une approche récursive.

## Voir aussi

- [Documentation officielle de clojure.string](https://clojure.github.io/clojure/clojure.string-api.html)
- [Exemples de code de manipulation de chaînes en Clojure](https://gist.github.com/Aluriak/5260891)
- [Tutoriel sur les fonctions récursives en Clojure](http://clojure-doc.org/articles/tutorials/recursion.html)