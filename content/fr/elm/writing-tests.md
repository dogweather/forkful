---
title:    "Elm: Ecrire des tests"
keywords: ["Elm"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/elm/writing-tests.md"
---

{{< edit_this_page >}}

# Pourquoi écrire des tests en Elm ?

Écrire des tests en Elm peut sembler fastidieux et prendre du temps, mais cela en vaut la peine. Les tests permettent de garantir que le code fonctionne correctement et d'éviter les erreurs tout au long du processus de développement. De plus, ils permettent de détecter plus facilement les bugs lors de la refactorisation ou de l'ajout de nouvelles fonctionnalités.

## Comment écrire des tests en Elm ?

Pour écrire des tests en Elm, il suffit d'utiliser la fonction `Test.test` et d'y placer le code à tester ainsi que le résultat attendu. Voici un exemple :

```Elm
import Test exposing (..)

add : Int -> Int
add x = x + 5

addTest =
    test "Additionne correctement" <|
        \() ->
            add 5
                |> Expect.equal 10

tests =
    [ addTest
    ]

main =
    run tests
```

Ici, nous avons créé une fonction `add` qui ajoute 5 à un nombre donné et un test qui vérifie si le résultat est bien égal à 10. Nous avons ensuite ajouté ce test à la liste `tests` et exécuté tous les tests en utilisant la fonction `run`.

## Plongée en profondeur

Il existe plusieurs types de tests en Elm, tels que les tests unitaires, les tests d'intégration et les tests d'acceptation. Il est important de comprendre quand et comment utiliser chacun de ces types de tests pour assurer une couverture complète du code.

Il est également possible de créer des mocks et des fakes en utilisant la bibliothèque `elm-explorations/test`, ce qui vous permet de simuler des données et des comportements pour tester différentes situations.

Enfin, il est important de noter qu'écrire des tests en Elm peut également aider à la documentation du code et à sa maintenabilité, en obligeant les développeurs à réfléchir à la façon dont ils veulent tester leur code et à écrire un code plus propre et plus testable.

# Voir aussi

- [Documentation officielle Elm pour les tests](https://guide.elm-lang.org/testing/)
- [Article sur l'écriture de tests en Elm](https://dev.to/roberttpetersen/introduction-to-testing-and-elm-glc)
- [Exemples de tests en Elm](https://github.com/mmachenry/elm-test-examples)