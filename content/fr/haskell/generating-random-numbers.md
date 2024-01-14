---
title:                "Haskell: Génération de nombres aléatoires"
simple_title:         "Génération de nombres aléatoires"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/haskell/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Pourquoi
La génération de nombres aléatoires est une compétence importante en programmation fonctionnelle, car elle permet de créer des simulations, des jeux et bien plus encore.

## Comment faire
```Haskell
import System.Random
import Control.Monad

main = do
    gen <- newStdGen
    let randomInts = randomRs (1, 10) gen :: [Int]
    print $ take 5 randomInts
```

Output: `[6,3,10,2,9]`

## Plongée en profondeur
La fonction `randomRs` génère une liste infinie de nombres aléatoires à partir d'un générateur de nombres aléatoires. En utilisant `take`, nous pouvons limiter le nombre de nombres aléatoires que nous souhaitons générer. De plus, en utilisant `newStdGen`, nous pouvons créer un nouveau générateur de nombres aléatoires à chaque exécution de notre programme pour obtenir des résultats différents à chaque fois.

## Voir aussi
- [Documentation Haskell sur la génération de nombres aléatoires](https://hackage.haskell.org/package/random-1.2.0/docs/System-Random.html)
- [Tutoriel vidéo sur la génération de nombres aléatoires en Haskell](https://www.youtube.com/watch?v=ZWR6ASQZpyM)