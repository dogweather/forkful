---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:30:24.784643-07:00
description: "\xC9crire des tests en Elm consiste \xE0 cr\xE9er des cas de test pour\
  \ v\xE9rifier la justesse de votre code Elm, en s'assurant qu'il se comporte comme\
  \ pr\xE9vu. Les\u2026"
lastmod: '2024-02-25T18:49:54.435081-07:00'
model: gpt-4-0125-preview
summary: "\xC9crire des tests en Elm consiste \xE0 cr\xE9er des cas de test pour v\xE9\
  rifier la justesse de votre code Elm, en s'assurant qu'il se comporte comme pr\xE9\
  vu. Les\u2026"
title: "R\xE9daction de tests"
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?

Écrire des tests en Elm consiste à créer des cas de test pour vérifier la justesse de votre code Elm, en s'assurant qu'il se comporte comme prévu. Les programmeurs le font pour attraper les bugs tôt, faciliter la maintenance, et améliorer la qualité et la fiabilité de leurs applications.

## Comment faire :

Elm utilise le paquet `elm-explorations/test` pour écrire des tests unitaires et de fuzz. Commencez par ajouter le paquet à votre projet :

```elm
elm install elm-explorations/test
```

Créez un fichier de test, disons `tests/ExampleTest.elm`, et importez les modules de test. Voici un test simple qui vérifie une fonction `add : Int -> Int -> Int` :

```elm
module ExampleTest exposant (..)

import Expect
import Test exposant (..)
import YourModuleName exposant (add)

suite : Test
suite =
    describe "Une simple fonction d'addition"
        [ test "Ajouter 2 et 3 donne 5" <| 
            \_ -> add 2 3 |> Expect.equal 5
        ]

```

Pour exécuter vos tests, vous aurez besoin de `elm-test` :

```shell
npm install -g elm-test
elm-test
```

Cela compilera vos tests et imprimera les résultats dans votre terminal. Pour l'exemple ci-dessus, le résultat devrait être quelque chose comme :

```
RUN DE TEST RÉUSSI

Durée : 42 ms
Réussis :   1
Échoués :   0
```

Pour un exemple plus complexe, disons que vous voulez tester la fonction `add` avec du fuzz pour vous assurer qu'elle gère correctement une large gamme d'entrées entières. Vous modifieriez votre `ExampleTest.elm` comme suit :

```elm
module ExampleTest exposant (..)

import Expect
import Fuzz exposant (int)
import Test exposant (..)
import YourModuleName exposant (add)

suite : Test
suite =
    describe "Tester add avec du fuzz"
        [ fuzz int "Test de fuzz sur add avec des entiers aléatoires" <| 
            \int1 int2 -> add int1 int2 |> Expect.equal (int1 + int2)
        ]
```

Exécutez `elm-test` à nouveau pour voir les tests de fuzz en action. Le résultat variera avec des entrées aléatoires mais des tests réussis indiqueront qu'il n'y a pas d'échecs :

```
RUN DE TEST RÉUSSI

Durée : 183 ms
Réussis :   100
Échoués :   0
``` 

Ces exemples montrent comment écrire et exécuter des tests unitaires simples et de fuzz en Elm, en utilisant le paquet `elm-explorations/test`. Les tests sont une partie vitale du processus de développement, aidant à assurer que vos applications Elm sont fiables et maintiennent une haute qualité.
