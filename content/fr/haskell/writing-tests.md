---
title:                "Haskell: Ecrire des tests"
simple_title:         "Ecrire des tests"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/haskell/writing-tests.md"
---

{{< edit_this_page >}}

## Pourquoi

L'écriture de tests est un aspect important de la programmation en Haskell car cela garantit la cohérence et la fiabilité de votre code. Les tests vous aident à détecter et à corriger les erreurs avant qu'elles ne deviennent des problèmes majeurs dans votre application.

## Comment faire

Pour écrire des tests en Haskell, nous utiliserons une librairie appelée HUnit. Voici un exemple de test simple pour une fonction qui renvoie le carré d'un nombre :

```Haskell
import Test.HUnit

square :: Int -> Int
square x = x * x

testSquare = TestCase (assertEqual "Should return 25" 25 (square 5))
```

Dans cet exemple, nous avons importé la librairie HUnit et défini une fonction 'square' qui renvoie le carré d'un nombre. Ensuite, nous avons défini un test, 'testSquare', qui vérifie si la fonction renvoie la valeur correcte pour un input de 5. La fonction 'assertEqual' vérifie si les deux valeurs données sont égales.

Pour exécuter ce test, nous pouvons utiliser la fonction 'runTestTT' :

```Haskell
main = runTestTT testSquare
```

Si tout fonctionne correctement, vous devriez voir une sortie comme celle-ci :

```Haskell
Cases: 1 Tried: 1 Errors: 0
Cases: 1 Tried: 1 Failures: 0
```

Cela signifie que notre test a réussi et que notre fonction 'square' fonctionne correctement !

## Plongée plus profonde

Maintenant que vous comprenez comment écrire des tests en Haskell, il est important de noter que les tests doivent être écrits pour couvrir tous les scénarios possibles. Cela inclut les inputs attendus, mais aussi les cas limites et les erreurs potentielles. Il est également important de s'assurer que les tests sont maintenus et mis à jour au fil du temps, car votre code évolue et change.

Une autre chose à garder à l'esprit est que les tests peuvent également être utilisés pour la documentation. Un bon test devrait décrire en détail ce que la fonction testée est censée faire et comment elle devrait être utilisée.

## Voir aussi

* [Documentation HUnit](https://hackage.haskell.org/package/HUnit)
* [Tutoriel pour écrire des tests avec HUnit](https://wiki.haskell.org/HUnit_for_beginners)
* [Bonne pratique pour écrire des tests en Haskell](https://www.stackbuilders.com/tutorials/haskell/testing-in-haskell/)