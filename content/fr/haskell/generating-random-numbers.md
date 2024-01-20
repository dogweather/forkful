---
title:                "Générer des nombres aléatoires"
html_title:           "Elixir: Générer des nombres aléatoires"
simple_title:         "Générer des nombres aléatoires"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/haskell/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Qu'est-ce que c'est & Pourquoi ?

Générer des nombres aléatoires, c'est produire une séquence de nombres qui n'a pas de motif visible. C'est crucial en programmation pour des choses comme la sécurité des données, les simulations et les jeux.

## Comment faire :

Vous pourrez produire des nombres aléatoires en Haskell en utilisant la bibliothèque `System.Random`. Regardons l'exemple suivant. 

```Haskell
import System.Random

randomNumber :: IO Int
randomNumber = getStdRandom (randomR (1, 100))

main = do 
    number <- randomNumber
    print(number)
```

Cela génère un nombre aléatoire entre 1 et 100. Lorsque vous exécutez ce programme, vous verrez la sortie qui varie à chaque fois.

```Haskell
>44
```
```Haskell
>80
```

## Plongée Profonde:

Historiquement, les ordinateurs ne sont pas capables de générer de vrais nombres aléatoires car ils sont complètement déterministes. Pour palier à cela, nous utilisons ce qu'on appelle des nombres pseudo-aléatoires qui semblent aléatoires, mais qui sont en réalité déterminés par un "seed" initial.

Il existe de nombreuses façons de générer des nombres aléatoires en Haskell, utilisons ici le générateur Mersenne Twister via la bibliothèque `random-fu`. Elle fournit des distributions de nombres aléatoires communes comme normale, exponentielle, et poisson.

```Haskell
import Data.Random
import Data.Random.Source.MWC

randomGaussian :: IO Double
randomGaussian = do
  g <- create 
  sampleFrom g (Normal (3.0::Double) 1.0)
```

La fonction `randomGaussian` génère des nombres aléatoires selon une distribution gaussienne.

## Voir Ausssi :

- Bibliothèque Haskell `System.Random` : http://hackage.haskell.org/package/random
- Bibliothèque Haskell `random-fu` : http://hackage.haskell.org/package/random-fu
- Pour une exploration plus approfondie des nombres aléatoires en Haskell : http://book.realworldhaskell.org/read/using-typeclasses.html