---
title:                "Création de nombres aléatoires"
html_title:           "Haskell: Création de nombres aléatoires"
simple_title:         "Création de nombres aléatoires"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/haskell/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Pourquoi

Si vous êtes intéressé par la programmation fonctionnelle, vous avez peut-être déjà entendu parler de Haskell. Et si vous vous êtes demandé pourquoi tant de développeurs l'utilisaient pour générer des nombres aléatoires, cet article est fait pour vous ! Dans ce guide, nous allons explorer comment générer des nombres aléatoires en Haskell et pourquoi c'est une pratique courante.

## Comment faire

Pour générer des nombres aléatoires en Haskell, nous allons utiliser le module `System.Random`. Tout d'abord, nous devons l'importer dans notre code :

```Haskell
import System.Random
```

Une fois que le module est importé, nous pouvons utiliser différentes fonctions pour générer des nombres aléatoires. Par exemple, la fonction `randomR` prend en paramètre un intervalle et renvoie un nombre aléatoire compris entre ces deux valeurs :

```Haskell
randomR (1,10) :: IO Int
-- Output: 5
```
Notez que la fonction renvoie un `IO Int`, ce qui signifie qu'elle doit être utilisée dans un contexte monadique. Si nous voulons simplement afficher le nombre généré, nous pouvons utiliser la fonction `randomRIO`, qui renvoie directement un `Int` :

```Haskell
randomRIO (1,10) :: Int
-- Output: 9
```

Nous pouvons également générer des nombres aléatoires avec des types de données plus complexes, comme des listes ou des tuples :

```Haskell
randomR ('a','z') :: IO Char
-- Output: 's'

randomR [1.0, 1.5, 2.0] :: IO Double
-- Output: 1.5

randomR (True, False) :: IO Bool
-- Output: True
```

Enfin, si nous voulons générer plusieurs nombres aléatoires, nous pouvons utiliser la fonction `randomRs`, qui génère une liste infinie de nombres aléatoires à partir d'un intervalle :

```Haskell
take 3 $ randomRs (1,10) :: [Int]
-- Output: [5, 6, 8]
```

## Plongée en profondeur

Maintenant que nous avons vu comment générer des nombres aléatoires en Haskell, il est important de comprendre un peu mieux comment cela fonctionne. Le module `System.Random` utilise un générateur de nombres pseudo-aléatoires, c'est-à-dire qu'il utilise un algorithme pour produire une série de nombres apparemment aléatoires. Cependant, ces nombres ne sont pas vraiment aléatoires, car ils peuvent être reproduits si l'on connaît la graine (seed) utilisée pour initialiser le générateur.

Cela signifie que, par défaut, chaque fois que nous appelons une fonction de génération de nombres aléatoires, elle utilise la même graine et produit la même séquence de nombres. Pour éviter cela, nous pouvons utiliser la fonction `getStdGen` pour récupérer une nouvelle graine à chaque fois que nous en avons besoin :

```Haskell
getStdGen :: IO StdGen
```

Il est également possible de générer une graine à partir d'une chaîne de caractères, ce qui permet de produire des séquences uniques en utilisant une graine différente pour chaque exécution du programme. Pour cela, nous pouvons utiliser la fonction `mkStdGen` :

```Haskell
mkStdGen :: Int -> StdGen
```

Il est important de noter que l'utilisation de générateurs de nombres pseudo-aléatoires peut entraîner des problèmes de sécurité dans certaines situations sensibles, comme pour des cryptographies ou pour des jeux d'argent. Dans ces cas, il est préférable d'utiliser un générateur de nombres vraiment aléatoires, tel que `/dev/urandom` sur les systèmes Unix.

## Voir aussi

- [Documentation officielle du module `System.Random`](https://hackage.haskell.org/package/random-1.2.0/docs/System-Random.html)
- [Tutoriel sur la génération de nombres aléatoires en Haskell](https://mmhaskell.com/blog/2017/9/13/random-generators)
- [Article sur la sécurité liée à l'utilisation de générateurs de nombres pseudo-aléatoires en Haskell](https://wiki.haskell.org/Random_shuffle#Security_note)