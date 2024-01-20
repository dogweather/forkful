---
title:                "Écriture de tests"
html_title:           "Haskell: Écriture de tests"
simple_title:         "Écriture de tests"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/haskell/writing-tests.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi?
Ecrire des tests est essentiel pour les programmeurs. Les tests sont des instructions écrites en code qui permettent de vérifier si le code fonctionne correctement. Cela peut aider à détecter et corriger les erreurs avant qu'elles n'affectent les utilisateurs finaux.

## Comment faire:
Il existe plusieurs façons d'écrire des tests en Haskell, mais voici une méthode simple utilisant le framework de test `HUnit`. Tout d'abord, importez le module `Test.HUnit` dans votre fichier : 
```Haskell
import Test.HUnit
```
Ensuite, définissez une fonction de test et définissez ses entrées et sa sortie attendue :
```Haskell
myTest :: Test
myTest = TestCase (assertEqual "2 + 2 devrait être égal à 4" 4 (2+2))
```
Enfin, exécutez le test en utilisant la fonction `runTestTT` et visualisez le résultat en utilisant la fonction `print` : 
```Haskell
main :: IO ()
main = do
    runTestTT myTest
    print "Tests terminés"
```
Lorsque vous exécutez ce code, vous devriez voir l'output suivant :
```markdown
Cases: 1  Tried: 1  Errors: 0  Failures: 0
"Tests terminés"
```

## Plongée en profondeur:
Les tests ont toujours été une partie importante du développement logiciel. Cela est particulièrement vrai pour les langages fonctionnels tels que Haskell, où la vérification statique des types ne suffit pas toujours à garantir la fiabilité du code. Il existe également d'autres frameworks de test en Haskell tels que `QuickCheck` et `HSpec` qui offrent une syntaxe plus expressive pour écrire des tests.

## Voir aussi:
- [Documentation officielle de HUnit](https://hackage.haskell.org/package/HUnit/docs/Test-HUnit.html)