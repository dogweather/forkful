---
title:    "Haskell: Génération de nombres aléatoires."
keywords: ["Haskell"]
---

{{< edit_this_page >}}

# Pourquoi générer des nombres aléatoires en Haskell ?

Les nombres aléatoires sont essentiels dans la programmation car ils permettent de créer des simulations, des jeux, des tests et bien plus encore. En Haskell, il existe des bibliothèques dédiées à la génération de nombres aléatoires, ce qui en fait un langage idéal pour ceux qui souhaitent travailler avec des données aléatoires.

## Comment générer des nombres aléatoires en Haskell

Pour générer des nombres aléatoires en Haskell, nous utiliserons la bibliothèque `System.Random`. Tout d'abord, nous devons importer cette bibliothèque dans notre code en utilisant la commande `import System.Random`. Ensuite, nous pouvons utiliser la fonction `random` pour générer un nombre aléatoire dans une plage donnée.

Voici un exemple de code pour générer un nombre aléatoire entre 1 et 10 :

```Haskell
import System.Random
main = do
  gen <- newStdGen
  let (randomNum, _) = randomR (1, 10) gen :: (Int, StdGen)
  print randomNum
```

La fonction `newStdGen` initialise un générateur de nombres aléatoires, tandis que la fonction `randomR` génère un nombre aléatoire dans la plage spécifiée. Ici, nous utilisons également le type `StdGen` pour garder une trace de notre générateur de nombres aléatoires.

## Plongez plus profondément dans la génération de nombres aléatoires en Haskell

La bibliothèque `System.Random` offre une variété de fonctions pour générer des nombres aléatoires avec différents types de données. Par exemple, la fonction `randomRIO` peut être utilisée pour générer un nombre aléatoire dans une plage spécifiée avec un type `IO`. De plus, il existe des fonctions pour générer des nombres aléatoires avec des types tels que `Double` et `Float`.

Il est également possible de définir nos propres générateurs de nombres aléatoires en utilisant la bibliothèque `System.Random.TF`. Cela peut être utile dans les cas où nous avons besoin de générateurs de nombres spécifiques pour des simulations ou des jeux.

# Voir aussi

- [Documentation sur la bibliothèque System.Random](https://hackage.haskell.org/package/random-1.2.0/docs/System-Random.html)
- [Tutoriel sur la génération de nombres aléatoires en Haskell](https://wiki.haskell.org/Haskell_random_numbers)
- [Documentation sur la bibliothèque System.Random.TF](https://hackage.haskell.org/package/random-tf-1.0.0.1/docs/System-Random-TF.html)