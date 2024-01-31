---
title:                "Rédaction de tests"
date:                  2024-01-19
html_title:           "Arduino: Rédaction de tests"
simple_title:         "Rédaction de tests"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/haskell/writing-tests.md"
---

{{< edit_this_page >}}

## What & Why?
Écrire des tests consiste à préparer des scénarios pour vérifier que notre code fonctionne bien. Les programmeurs écrivent des tests pour prévenir des erreurs, garantir la qualité et faciliter les mises à jour de code sans casser des fonctionnalités existantes.

## How to:
Haskell utilise Hspec pour des tests. C'est simple et intuitif :

```haskell
import Test.Hspec

-- Une fonction simple pour l'exemple
add :: Int -> Int -> Int
add x y = x + y

-- Les tests
main :: IO ()
main = hspec $ do
  describe "add" $ do
    it "additionne deux nombres positifs" $
      add 2 3 `shouldBe` 5

    it "additionne un positif et un négatif" $
      add (-1) 1 `shouldBe` 0
```

Résultat après exécution :
```
add
  additionne deux nombres positifs
  additionne un positif et un négatif

Finished in 0.0003 seconds
2 examples, 0 failures
```

## Deep Dive
Historiquement, QuickCheck était l'outil dominant pour les tests en Haskell, introduisant l'approche de tests basée sur les propriétés. Hspec est une alternative qui propose une syntaxe plus lisible basée sur des exemples. La spécificité de ces tests en Haskell, c'est qu'on peut combiner les tests basés sur des exemples (Hspec) avec des tests basés sur les propriétés (QuickCheck).

## See Also
Pour aller plus loin :

- [Hspec documentation](http://hspec.github.io/)
- [QuickCheck sur Hackage](https://hackage.haskell.org/package/QuickCheck)
