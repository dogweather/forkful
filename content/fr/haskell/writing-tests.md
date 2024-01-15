---
title:                "Écrire des tests"
html_title:           "Haskell: Écrire des tests"
simple_title:         "Écrire des tests"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/haskell/writing-tests.md"
---

{{< edit_this_page >}}

# Pourquoi

Au cours de notre parcours en tant que développeurs, nous avons probablement tous entendu parler de l'importance d'écrire des tests pour notre code. Mais pourquoi est-ce si important ? 

Ecrire des tests permet de s'assurer que notre code fonctionne correctement et continue de fonctionner correctement au fil des modifications ou ajouts ultérieurs. Cela nous permet également de détecter rapidement et résoudre les bugs, ce qui peut nous faire économiser beaucoup de temps et d'efforts à long terme.

# Comment Faire

Ecrire des tests en Haskell est assez simple grâce à l'utilisation du module de test standard du langage, `Test.HUnit`. Voici un exemple de test pour une fonction `multiply` qui multiplie deux nombres :

```
Haskell
module Tests where

import Test.HUnit

-- Définition de notre fonction multiply
multiply :: Int -> Int -> Int
multiply x y = x * y

-- Les tests
tests :: Test
tests = TestList [
    -- Test de la multiplication de deux nombres positifs
    TestCase $ assertEqual "Multilication de 2 et 4" 8 (multiply 2 4),
    -- Test de la multiplication de deux nombres négatifs
    TestCase $ assertEqual "Multiplication de -3 et -5" 15 (multiply (-3) (-5)),
    -- Test de la multiplication par zéro
    TestCase $ assertEqual "Multiplication par zéro" 0 (multiply 0 10)
    ]

-- Exécution des tests
main :: IO()
main = do
    runTestTT tests
```

L'output résultant devrait être :

```
Start running 3 test cases...
Cases run: 3
Guesses: 0
Asserts: Succeeded 3, Failed 0.
```

Nous pouvons voir que tous nos tests ont réussi et cela nous donne ainsi la confiance que notre fonction `multiply` fonctionne correctement.

# Plongée en Profondeur

Il existe différents types de tests que nous pouvons écrire en Haskell tels que les tests unitaires, les tests d'intégration et les tests de propriétés. Nous pouvons également utiliser des outils comme QuickCheck pour générer des données aléatoires et tester nos fonctions avec celles-ci.

En écrivant des tests, nous devons nous assurer de couvrir différents scénarios, tels que les cas limites (comme la multiplication par zéro dans notre exemple) et les cas d'erreur. Nous devons également nous assurer que nos tests sont indépendants les uns des autres et qu'ils ne dépendent pas de l'ordre d'exécution.

# Voir Aussi

- [Documentation du module de test standard de Haskell](https://hackage.haskell.org/package/HUnit)
- [Introduction aux tests en Haskell](https://en.wikibooks.org/wiki/Haskell/Testing)
- [QuickCheck pour générer des données aléatoires en Haskell](https://hackage.haskell.org/package/QuickCheck)