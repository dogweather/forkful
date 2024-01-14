---
title:    "Haskell: Génération de nombres aléatoires"
keywords: ["Haskell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/haskell/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Pourquoi

Les nombres aléatoires sont souvent utilisés dans la programmation pour créer des défis différents et uniques à chaque exécution d'un programme. Cela peut être utile dans les tests et les simulations, ainsi que dans les jeux où la variété et l'incertitude sont importantes. En utilisant Haskell, nous pouvons facilement générer des nombres aléatoires et les utiliser dans nos programmes.

## Comment faire

Générer des nombres aléatoires en Haskell est assez simple en utilisant le module `System.Random`. Tout d'abord, nous devons importer ce module dans notre code avec la ligne suivante:

```Haskell
import System.Random
```

Ensuite, nous pouvons utiliser la fonction `randomR` pour générer un nombre aléatoire dans une plage donnée. Par exemple, pour générer un nombre aléatoire entre 1 et 10, nous pouvons écrire le code suivant:

```Haskell
randomNumber <- randomR (1, 10)
```

Ici, `randomNumber` sera lié à une valeur entière aléatoire entre 1 et 10 à chaque exécution du programme.

Nous pouvons également générer des listes de nombres aléatoires en utilisant la fonction `randomRs`. Par exemple, pour créer une liste de 5 nombres entiers aléatoires entre 1 et 100, nous pouvons écrire:

```Haskell
randomNumbers <- take 5 $ randomRs (1, 100) (mkStdGen 123)
```

La fonction `mkStdGen` prend un entier en paramètre pour initialiser le générateur de nombre aléatoire. Cela garantit que nous obtenons la même liste de nombres aléatoires à chaque exécution du programme.

## Plongée profonde

En utilisant le module `System.Random`, nous pouvons générer des nombres pseudo-aléatoires. Cela signifie que les nombres générés semblent aléatoires mais sont en fait déterminés par une formule mathématique. Pour générer des nombres vraiment aléatoires, nous devons utiliser une source externe telle qu'un générateur de nombres aléatoires matériel.

De plus, il est important de noter que la fonction `randomR` utilise une distribution uniforme pour générer des nombres aléatoires, ce qui signifie que chaque nombre a la même probabilité d'être généré. Si nous voulons une distribution différente, nous pouvons utiliser des fonctions telles que `random`, `randomRIO` ou `randomRs` avec une distribution spécifiée.

## Voir aussi

- [Documentation sur le module System.Random](https://hackage.haskell.org/package/random-1.2.0/docs/System-Random.html)
- [Tutorial vidéo sur les nombres aléatoires en Haskell](https://www.youtube.com/watch?v=bAKtCcTiI6M)