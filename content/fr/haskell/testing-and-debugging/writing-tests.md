---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:30:45.537260-07:00
description: "\xC9crire des tests en Haskell, c'est s'assurer que vos fonctions fonctionnent\
  \ comme pr\xE9vu gr\xE2ce \xE0 des v\xE9rifications automatis\xE9es. Les programmeurs\
  \ le font\u2026"
lastmod: '2024-02-25T18:49:54.560263-07:00'
model: gpt-4-0125-preview
summary: "\xC9crire des tests en Haskell, c'est s'assurer que vos fonctions fonctionnent\
  \ comme pr\xE9vu gr\xE2ce \xE0 des v\xE9rifications automatis\xE9es. Les programmeurs\
  \ le font\u2026"
title: "R\xE9daction de tests"
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?

Écrire des tests en Haskell, c'est s'assurer que vos fonctions fonctionnent comme prévu grâce à des vérifications automatisées. Les programmeurs le font pour détecter les bugs dès le début, faciliter le refactoring et documenter le comportement, rendant la base de code plus maintenable et évolutive.

## Comment faire :

Haskell prend en charge divers cadres de test, mais deux populaires sont `Hspec` et `QuickCheck`. Hspec vous permet de définir des spécifications lisibles par l'humain pour votre code, tandis que QuickCheck vous permet de générer automatiquement des tests en décrivant des propriétés que votre code devrait satisfaire.

### Utiliser Hspec

D'abord, ajoutez `hspec` à la configuration de votre outil de build (par exemple, `stack.yaml` ou fichier `cabal`). Ensuite, importez `Test.Hspec` et écrivez des tests comme spécifications :

```haskell
-- fichier : spec/MyLibSpec.hs
import Test.Hspec
import MyLib (add)

main :: IO ()
main = hspec $ describe "MyLib.add" $ do
  it "ajoute deux nombres" $
    add 1 2 `shouldBe` 3

  it "retourne le premier nombre lors de l'ajout de zéro" $
    add 5 0 `shouldBe` 5
```

Ensuite, exécutez vos tests en utilisant votre outil de build, ce qui donnera un résultat qui pourrait ressembler à :

```
MyLib.add
  - ajoute deux nombres
  - retourne le premier nombre lors de l'ajout de zéro

Terminé en 0.0001 secondes
2 exemples, 0 échecs
```

### Utiliser QuickCheck

Avec QuickCheck, vous exprimez des propriétés que vos fonctions devraient satisfaire. Ajoutez `QuickCheck` à la configuration de votre projet, puis importez-le :

```haskell
-- fichier : test/MyLibProperties.hs
import Test.QuickCheck
import MyLib (add)

prop_addAssociative :: Int -> Int -> Int -> Bool
prop_addAssociative x y z = x + (y + z) == (x + y) + z

prop_addCommutatif :: Int -> Int -> Bool
prop_addCommutatif x y = x + y == y + x

main :: IO ()
main = do
  quickCheck prop_addAssociative
  quickCheck prop_addCommutatif
```

Exécuter ces tests générera automatiquement des entrées pour vérifier les propriétés spécifiées :

```
+++ OK, passé 100 tests.
+++ OK, passé 100 tests.
```

Dans les exemples Hspec et QuickCheck, les suites de tests servent de documentation exécutable qui peut vérifier automatiquement la correction de votre code.
