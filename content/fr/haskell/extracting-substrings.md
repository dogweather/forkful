---
title:                "Extraction de sous-chaînes"
html_title:           "Haskell: Extraction de sous-chaînes"
simple_title:         "Extraction de sous-chaînes"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/haskell/extracting-substrings.md"
---

{{< edit_this_page >}}

# Pourquoi

Extraction de sous-chaînes de caractères: Pourquoi le faire?

Si vous avez déjà eu à manipuler des chaînes de caractères dans vos programmes, vous avez probablement rencontré des situations où vous avez besoin d'extraire une partie spécifique de la chaîne. Peut-être que vous voulez obtenir un nom de fichier à partir d'un chemin complet ou extraire un numéro de téléphone d'un texte. Dans ces cas, avoir une méthode pour extraire facilement des sous-chaînes peut être très utile.

# Comment faire

Pour extraire des sous-chaînes en Haskell, nous allons utiliser la fonction `take` et `drop` de la bibliothèque standard. `take` prend en entrée un nombre entier `n` et une liste et renvoie une sous-liste des `n` premiers éléments de la liste. De même, `drop` prend en entrée un nombre entier `n` et une liste et renvoie une sous-liste des éléments de la liste à partir de `n`. En combinant ces deux fonctions, nous pouvons extraire une partie spécifique d'une chaîne de caractères. Voici un exemple de code:

```Haskell
let str = "Bonjour les programmeurs!"
let nom = take 7 str -- renvoie "Bonjour"
let message = drop 8 str -- renvoie "les programmeurs!"
```

Pour extraire une sous-chaîne entre deux positions spécifiques, nous pouvons utiliser la fonction `takeDrop` définie dans la bibliothèque `Data.List.Split`. Cette fonction prend en entrée une liste, une position de départ et une position de fin, et renvoie une sous-liste de la liste qui se situe entre ces positions. Voici un exemple d'utilisation:

```Haskell
import Data.List.Split
let str = "Hello world!"
let sousChaine = takeDrop str 2 6 -- renvoie "llo w"
```


# Plongée en profondeur

Maintenant que nous savons comment extraire des sous-chaînes en Haskell, voyons comment cela fonctionne en détail. La fonction `take` utilise des nombres entiers pour déterminer combien d'éléments de la liste doivent être pris. Si le nombre entier `n` est plus grand que la longueur de la liste, `take` renverra simplement toute la liste. Sinon, elle renverra une liste avec les `n` premiers éléments. D'un autre côté, `drop` ignore simplement les `n` premiers éléments et renvoie le reste de la liste.

Pour extraire une sous-chaîne entre deux positions spécifiques, nous utilisons la fonction `takeDrop`. Dans cette fonction, nous utilisons simplement les fonctions `take` et `drop` que nous avons déjà vues, mais avec des positions spécifiques en tant que paramètres.

# Voir aussi

- [Documentation sur la fonction `take`](https://hackage.haskell.org/package/base-4.14.1.0/docs/Data-List.html#v:take)
- [Documentation sur la fonction `drop`](https://hackage.haskell.org/package/base-4.14.1.0/docs/Data-List.html#v:drop)
- [Documentation sur `Data.List.Split`](https://hackage.haskell.org/package/split-0.2.3/docs/Data-List-Split.html)