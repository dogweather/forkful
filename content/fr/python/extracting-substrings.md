---
title:    "Python: Extraction de sous-chaînes"
keywords: ["Python"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/python/extracting-substrings.md"
---

{{< edit_this_page >}}

Pourquoi:

Extraction de sous-chaînes en Python

Si vous travaillez avec des chaînes de caractères en Python, vous avez peut-être remarqué que parfois vous devez extraire une sous-chaîne à partir d'une chaîne plus grande. Cela peut être utile lors de la manipulation de données ou de la création de fonctions pour traiter les chaînes de manière spécifique. Dans cet article, nous allons explorer comment extraire des sous-chaînes en Python.

Comment faire:

Pour extraire une sous-chaîne en Python, nous pouvons utiliser la méthode `slice()` ou l'opérateur de tranche `[:]`. Par exemple, supposons que nous avons la chaîne de caractères "Bonjour à tous" et que nous voulons extraire la sous-chaîne "à tous". Nous pouvons le faire en utilisant la méthode `slice()` de cette façon:

 ```
 phrase = "Bonjour à tous"
 sous_chaine = phrase.slice(8, 14)
 print(sous_chaine)
 ```

Cela nous donnera la sortie suivante:

`à tous`

Nous pouvons également utiliser l'opérateur de tranche `[:]` de cette façon:

 ```
 phrase = "Bonjour à tous"
 sous_chaine = phrase[8:14]
 print(sous_chaine)
 ```

Cela nous donnera la même sortie que précédemment. L'opérateur de tranche utilise les mêmes indices que la méthode `slice()`, mais est plus concis et pratique à utiliser.

Deep Dive:

La méthode `slice()` prend en compte trois arguments: `start`, `end` et `step`. `start` représente l'indice de départ de la sous-chaîne, `end` représente l'indice de fin et `step` représente le pas. Le pas est optionnel et par défaut, il est égal à 1. Cela signifie que la sous-chaîne extraite comprendra tous les caractères entre les indices `start` et `end`. Cependant, si nous spécifions un pas différent de 1, nous pouvons obtenir une sous-chaîne avec chaque n-ème caractère.

Par exemple, si nous voulons extraire tous les autres caractères de la chaîne "Bonjour à tous", nous pouvons utiliser un pas de 2 de cette manière:

```
phrase = "Bonjour à tous"
sous_chaine = phrase.slice(0, 14, 2)
print(sous_chaine)
```

La sortie sera alors:

`Bno àtu`

Cela peut être utile dans certaines situations spécifiques.

Voir aussi:

- https://www.geeksforgeeks.org/python-string-slice/
- https://www.tutorialspoint.com/python/string_slice.htm
- https://www.programiz.com/python-programming/methods/string/slice