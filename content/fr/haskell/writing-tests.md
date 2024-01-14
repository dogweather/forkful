---
title:                "Haskell: Écrire des tests"
programming_language: "Haskell"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/haskell/writing-tests.md"
---

{{< edit_this_page >}}

## Pourquoi

Les tests sont un outil essentiel pour tout programmeur, quel que soit le langage de programmation utilisé. En écrivant des tests, vous vous assurez que votre code fonctionne correctement et que les modifications futures ne cassent pas votre application. Cela vous fait gagner du temps et évite également les problèmes potentiels pour les utilisateurs de votre application.

## Comment faire

Pour écrire des tests en Haskell, vous aurez besoin d'une bibliothèque appelée HUnit. Vous pouvez l'installer en utilisant le gestionnaire de paquets Cabal avec la commande suivante :

`cabal install HUnit`

Ensuite, vous pourrez utiliser HUnit pour écrire vos tests en utilisant la syntaxe suivante :

```Haskell
testFonction = TestCase(assertEqual "message d'erreur" expected actual)
```

Dans cet exemple, "testFonction" est le nom de votre test, "assertEqual" est la fonction de vérification que vous utilisez pour comparer le résultat attendu "expected" avec le résultat réel "actual", et "message d'erreur" est un message qui sera affiché si le test échoue. Vous pouvez ensuite écrire plusieurs tests et les regrouper dans une suite de tests en utilisant la fonction "TestList" :

```Haskell
suiteTests = TestList [testFonction1, testFonction2, testFonction3]
```

Enfin, pour exécuter votre suite de tests, vous pouvez utiliser la fonction "runTestTT" :

```Haskell
main = runTestTT suiteTests
```

Vous verrez ensuite le résultat de vos tests dans la console.

## Plongée en profondeur

Il est important de noter que vous devez tester non seulement votre code, mais également les bordures de celui-ci. Cela signifie que vous devez tester les cas où votre code est censé échouer, ainsi que les cas où il est censé réussir. De plus, vous pouvez également utiliser des frameworks de test intégrés tels que QuickCheck ou SmallCheck pour tester des propriétés plus générales de votre code.

## Voir aussi

- Site officiel de HUnit : https://hackage.haskell.org/package/HUnit
- Documentation de HUnit : https://hackage.haskell.org/package/HUnit/docs/Test-HUnit-Base.html
- Tutoriel sur les tests en Haskell : http://learnyouahaskell.com/unit-testing#introducing-hunit