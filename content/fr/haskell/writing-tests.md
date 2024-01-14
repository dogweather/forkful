---
title:    "Haskell: Écrire des tests"
keywords: ["Haskell"]
---

{{< edit_this_page >}}

## Pourquoi 

Lorsque nous créons des programmes, il est crucial de s'assurer qu'ils fonctionnent correctement et de manière fiable. Les tests sont un moyen efficace pour garantir la qualité de notre code et détecter les erreurs potentielles avant qu'elles ne causent des problèmes dans un environnement de production.

## Comment faire 

Pour écrire des tests en Haskell, nous pouvons utiliser le framework de test HSpec. Tout d'abord, nous devons importer le module `Test.HSpec` avec la directive `import Test.HSpec` puis définir notre suite de tests avec la fonction `hspec` qui prend en argument une fonction qui spécifie nos tests.

Voici un exemple de test pour une fonction `estPair` qui vérifie si un nombre est pair :

```Haskell
import Test.Hspec

estPair :: Int -> Bool
estPair n = (n `mod` 2) == 0

main :: IO ()
main = hspec $ do
 describe "estPair" $
   it "retourne True si le nombre est pair" $
     estPair 4 `shouldBe` True
   it "retourne False si le nombre est impair" $
     estPair 3 `shouldBe` False
```

Nous pouvons exécuter nos tests en utilisant la commande `runhaskell` suivie du nom de notre fichier contenant les tests. Si tous les tests passent, nous devrions voir une sortie similaire à ceci :

```
estPair
  √ retourne True si le nombre est pair
  √ retourne False si le nombre est impair

Finished in 0.0008 seconds
2 examples, 0 failures
```

## Plongée en profondeur 

Il existe différents types de tests que nous pouvons écrire en Haskell, tels que les tests unitaires, les tests d'intégration et les tests de propriété. Les tests unitaires vérifient le comportement de petits morceaux de code, tandis que les tests d'intégration vérifient l'interaction entre plusieurs parties de notre programme. Les tests de propriété utilisent des propriétés spécifiques pour vérifier le comportement de notre code et peuvent être particulièrement utiles pour détecter des erreurs non évidentes.

Il est également important de noter que les tests sont un outil continu et doivent être révisés et mis à jour au fur et à mesure que notre code évolue. Nous pouvons également utiliser des outils d'analyse de couverture de code pour nous assurer que nos tests couvrent suffisamment de cas pour garantir la qualité de notre code.

## Voir aussi 

- [Tutoriel HSpec](https://hspec.github.io)
- [Exemples de tests en Haskell](https://haskellweekly.news/issue/134.html)
- [Analyse de couverture de code avec HPC](https://www.fpcomplete.com/blog/2017/07/easier-correlation-of-tests-and-code-in-haskell)