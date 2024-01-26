---
title:                "Rédaction de tests"
html_title:           "Arduino: Rédaction de tests"
simple_title:         "Rédaction de tests"
programming_language: "Elm"
category:             "Elm"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/elm/writing-tests.md"
---

{{< edit_this_page >}}

## What & Why?
Les tests sont des vérifications automatisées de votre code. Ils préviennent les bugs et assurent que tout fonctionne après des modifications. C'est une assurance qualité pour votre projet.

## How to:

Pour écrire des tests en Elm, utilisez `elm-test`. Voici un exemple simple :

```Elm
import Expect
import Test exposing (..)

suiteDeTests : Test
suiteDeTests =
    describe "Un exemple de test"
        [ test "addition simple" <|
            \_ -> 2 + 2 |> Expect.equal 4
        ]

-- Pour lancer les tests :
-- $ elm-test
```

Sortie attendue :

```
TEST RUN PASSED

Un exemple de test
    ✓ addition simple

1 test run, 0 failures.
```

## Deep Dive

Les tests en Elm ont été influencés par les pratiques de développement logiciel comme TDD. Les alternatives populaires incluent QuickCheck pour les tests de propriétés. Elm a intégré le système de types pour minimiser les bugs, mais les tests restent essentiels pour valider la logique.

## See Also

- [Guide officiel elm-test](https://package.elm-lang.org/packages/elm-explorations/test/latest/)
- [Elm Test Runner pour l'intégration avec CI](https://github.com/rtfeldman/node-test-runner)
