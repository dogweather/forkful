---
title:                "Écriture de tests"
html_title:           "Elm: Écriture de tests"
simple_title:         "Écriture de tests"
programming_language: "Elm"
category:             "Elm"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/elm/writing-tests.md"
---

{{< edit_this_page >}}

## Pourquoi

Si vous êtes un développeur Elm, vous avez probablement entendu parler des tests automatisés. Mais pourquoi devriez-vous en écrire ? Eh bien, le principal objectif des tests est de s'assurer que votre code fonctionne correctement et avec cohérence, même après de multiples modifications. Des tests bien écrits peuvent vous faire gagner un temps précieux en détectant rapidement les erreurs et en vous évitant des heures de débogage.

## Comment faire

Pour écrire des tests en Elm, vous devez utiliser le module `elm-test`. Tout d'abord, vous devez installer `elm-test` en utilisant la commande `elm install elm-explorations/test`. Ensuite, vous pouvez écrire vos tests dans un fichier séparé avec l'extension `.elm`. Voici un exemple de test pour une fonction de multiplication :

```
module Test exposing (..)

import Expect exposing (expect)
import MyModule exposing (multiply)

testMultiply : Test
testMultiply =
    describe "Multiply function"
        [ test "Multiplying 5 * 5 should return 25" <|
            \() ->
                expect (multiply 5 5) toEqual 25
        ]
```

Dans cet exemple, nous importons le module `Expect`, qui contient des fonctions pour vérifier nos résultats de test. Nous importons également notre module `MyModule`, qui contient la fonction `multiply` que nous voulons tester. Ensuite, nous définissons notre test en utilisant la fonction `test` et en lui donnant un nom et une fonction qui évalue le résultat attendu. Nous pouvons ensuite exécuter nos tests en utilisant la commande `elm-test` dans notre terminal.

## Approfondissement

Maintenant que vous savez comment écrire des tests en Elm, vous pouvez explorer différentes façons de les utiliser. Par exemple, vous pouvez utiliser des tests pour guider votre processus de développement en écrivant d'abord les tests, puis en écrivant le code pour les faire réussir. Vous pouvez également utiliser des tests pour mesurer la couverture de code de vos applications. Enfin, vous pouvez utiliser des tests pour détecter les erreurs dans votre code lorsque vous apportez des modifications à votre application.

## Voir aussi

- [Documentation du module Elm-Test](https://package.elm-lang.org/packages/elm-explorations/test/latest/)
- [Article "Introduction to Testing in Elm" de NoRedInk Engineering](https://tech.noredink.com/post/152489705573/introduction-to-testing-in-elm)