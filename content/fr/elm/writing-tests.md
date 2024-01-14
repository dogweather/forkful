---
title:                "Elm: Écriture des tests"
simple_title:         "Écriture des tests"
programming_language: "Elm"
category:             "Elm"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/elm/writing-tests.md"
---

{{< edit_this_page >}}

# Pourquoi écrire des tests en Elm ?

Écrire des tests pour son code peut sembler fastidieux et prendre du temps supplémentaire, mais cela peut apporter de nombreux avantages à long terme. Tout d'abord, les tests permettent de s'assurer que le code fonctionne correctement et qu'il n'y a pas de bugs potentiels. Ils peuvent également aider à détecter les erreurs de manière plus rapide et efficace lors de la phase de développement. Enfin, les tests fournissent une documentation précise sur le fonctionnement du code, permettant aux développeurs de mieux comprendre leur propre code et de pouvoir le modifier plus facilement à l'avenir.

# Comment écrire des tests en Elm ?

Ecrire des tests en Elm est assez simple et peut se faire en utilisant le framework de tests intégré de Elm, appelé "elm-test". Voici un exemple de comment un test peut être écrit en Elm :

```Elm
import Html
import Test exposing (..)

sum : Int -> Int -> Int
sum x y =
    x + y

tests : Test
tests =
    describe "Test de la fonction sum" [
        test "sum 2 + 2 égal à 4" <|
            \_ ->
                Expect.equal (sum 2 2) 4
    ]

main : Html.Html a
main =
    Html.text (if (run tests).passed then "Tous les tests ont réussi !" else "Un ou plusieurs tests ont échoué...")
```

Dans cet exemple, nous importons le framework de tests et la librairie Html pour créer un petit résultat visuel. Ensuite, nous définissons une fonction "sum" qui ajoute deux entiers et nous écrivons un test pour vérifier si le résultat est bien égal à 4. Enfin, nous exécutons le test et affichons un message en fonction du résultat. En utilisant "elm-test" et la fonction "Expect", il est facile d'écrire des tests pour son code en Elm.

# Plongée en profondeur

L'écriture de tests en Elm suit le principe de programmation "Test Driven Development" (ou TDD) qui consiste à écrire les tests avant même d'écrire le code. Cela peut sembler contre-intuitif, mais cela permet d'avoir un code plus robuste et cohérent, ainsi qu'une meilleure compréhension du problème à résoudre avant même de commencer à coder. De plus, les tests doivent être régulièrement mis à jour et maintenus, afin de garantir que le code reste fonctionnel à mesure qu'il évolue.

# Voir aussi

- [Guide officiel de Elm sur l'écriture de tests](https://guide.elm-lang.org/testing/)
- [Documentation du framework de tests "elm-test"](https://package.elm-lang.org/packages/elm-explorations/test/latest/)
- [Vidéo explicative sur le TDD en Elm](https://www.youtube.com/watch?v=XYEqWnp4h2A)