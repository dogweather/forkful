---
title:                "Haskell: Extraction de sous-chaînes"
simple_title:         "Extraction de sous-chaînes"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/haskell/extracting-substrings.md"
---

{{< edit_this_page >}}

## Pourquoi

Dans la programmation Haskell, il est souvent nécessaire de travailler avec des chaînes de caractères. L'extraction de sous-chaînes à partir d'une chaîne existante peut être utile pour effectuer des manipulations ou des vérifications spécifiques. Dans cet article, nous allons explorer pourquoi et comment extraire des sous-chaînes en Haskell.

## Comment faire

Nous pouvons utiliser la fonction `take` pour extraire une sous-chaîne à partir d'une chaîne en spécifiant le nombre de caractères à extraire. Par exemple :

```Haskell
let str = "Bonjour le monde"
take 7 str -- renvoie "Bonjour"
```

Nous pouvons également utiliser la fonction `drop` pour supprimer une partie d'une chaîne en spécifiant le nombre de caractères à supprimer. Par exemple :

```Haskell
let str = "Bonjour le monde"
drop 7 str -- renvoie "le monde"
```

Pour extraire une sous-chaîne spécifique en fonction d'un index de début et d'un index de fin, nous pouvons utiliser la fonction `take` suivie de `drop`. Par exemple :

```Haskell
let str = "Bonjour le monde"
let debut = 3 -- index de début
let fin = 9 -- index de fin
drop debut (take fin str) -- renvoie "jour l"
```

## Plongée en profondeur

La fonction `take` renvoie une sous-chaîne à partir du début de la chaîne, tandis que la fonction `drop` renvoie une sous-chaîne à partir de la fin de la chaîne. En combinant ces deux fonctions, nous pouvons extrêmement manipuler une chaîne et extraire des sous-chaînes avec précision.

Il est également important de noter que la numérotation des indices commence à partir de 0 en Haskell. Ainsi, le premier caractère d'une chaîne a un index de 0.

## Voir aussi

- La documentation officielle sur la manipulation des chaînes en Haskell : https://hackage.haskell.org/package/base-4.12.0.0/docs/Data-String.html
- Un tutoriel complet sur la manipulation des chaînes en Haskell : https://www.tutorialspoint.com/haskell/haskell_strings.htm
- Un article sur les fonctions de manipulation des chaînes en Haskell : http://haskell.tailorfontela.com.br/string-manipulation-functions/