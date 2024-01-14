---
title:                "Haskell: Générer des nombres aléatoires"
programming_language: "Haskell"
category:             "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/haskell/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Pourquoi

Les nombres aléatoires sont souvent utilisés dans la programmation pour simuler une variété d'événements, tels que des jeux ou des expériences aléatoires. Cela peut rendre vos programmes plus dynamiques et intéressants pour les utilisateurs. En utilisant Haskell, vous pouvez facilement générer des nombres aléatoires pour vos projets de programmation.

## Comment faire

Pour générer des nombres aléatoires en Haskell, vous devez importer le module `System.Random`, qui fournit des fonctions pour générer des valeurs aléatoires. Voici un exemple simple de génération de 5 nombres aléatoires entre 1 et 10 :

```Haskell
import System.Random

main = do
    gen <- getStdGen
    let randomNumbers = take 5 (randomRs (1,10) gen :: [Int])
    print randomNumbers
```
La fonction `getStdGen` renvoie un générateur de nombres aléatoires qui utilise l'horloge du système pour initialiser la séquence de nombres. La fonction `randomRs` prend un intervalle de nombres et un générateur en entrée, et renvoie une liste infinie de nombres aléatoires dans cet intervalle. En utilisant `take 5`, nous limitons cette liste à 5 éléments. Le paramètre `:: [Int]` spécifie que les valeurs dans la liste seront des entiers.

Lorsque vous exécutez ce code, vous obtiendrez une liste de 5 entiers aléatoires, par exemple `[7,3,8,2,6]`. Chaque fois que vous exécutez le code, vous obtiendrez une nouvelle séquence de nombres aléatoires en raison de l'utilisation de l'horloge système pour initialiser le générateur.

## Plongée en profondeur

Il est également possible de générer des nombres aléatoires à l'aide d'un générateur personnalisé en utilisant la fonction `mkStdGen`. Cela peut être utile si vous voulez obtenir la même séquence de nombres aléatoires à chaque exécution de votre programme.

En utilisant la fonction `randomR` au lieu de `randomRs`, vous pouvez générer un seul nombre aléatoire dans un intervalle donné. Voici un exemple de génération d'un nombre aléatoire entre 1 et 100 :

```Haskell
import System.Random

main = do
    gen <- newStdGen
    let randomNumber = randomR (1,100) gen :: Int
    print randomNumber
```

De plus, vous pouvez également générer des nombres aléatoires pour différents types de données, tels que des floats et des booléens, en utilisant les fonctions `randomR` et `randomRs`.

## Voir aussi

- [Documentation officielle de System.Random en Haskell](https://hackage.haskell.org/package/random/docs/System-Random.html)
- [Un tutoriel approfondi sur la génération de nombres aléatoires en Haskell](https://mmhaskell.com/blog/2017/5/15/generating-random-values-in-haskell)
- [Un exemple pratique de génération de nombres aléatoires en Haskell pour un jeu de dés](https://github.com/Antheizme/haskell-dice-roll)