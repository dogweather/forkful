---
aliases:
- /fr/elm/organizing-code-into-functions/
date: 2024-01-26 01:09:54.283376-07:00
description: "Entasser tout votre code dans un grand tas ? Mauvaise id\xE9e. Le d\xE9\
  couper en fonctions ? Bonne id\xE9e. Cela permet de garder votre code Elm propre,\u2026"
lastmod: 2024-02-18 23:09:08.740607
model: gpt-4-1106-preview
summary: "Entasser tout votre code dans un grand tas ? Mauvaise id\xE9e. Le d\xE9\
  couper en fonctions ? Bonne id\xE9e. Cela permet de garder votre code Elm propre,\u2026"
title: Organisation du code en fonctions
---

{{< edit_this_page >}}

## Quoi et Pourquoi ?
Entasser tout votre code dans un grand tas ? Mauvaise idée. Le découper en fonctions ? Bonne idée. Cela permet de garder votre code Elm propre, réutilisable et plus facile à tester. En organisant votre code en fonctions, vous regroupez le code qui effectue des tâches spécifiques ensemble, ce qui rend votre application plus maintenable et compréhensible.

## Comment faire :
Voici un extrait de code Elm avec une fonction simple pour saluer un utilisateur :

```Elm
module Main exposing (..)

import Html exposing (text)

greetUser : String -> String
greetUser userName =
    "Bonjour, " ++ userName ++ "!"

main =
    text (greetUser "Casey")
```

Exécutez-le, et vous obtiendrez la sortie : "Bonjour, Casey !"

Maintenant, disons que vous voulez ajouter plus de personnalisation. Extrayez plus de fonctionnalités !

```Elm
module Main exposing (..)

import Html exposing (text)

greetUser : String -> String -> String
greetUser greeting userName =
    greeting ++ ", " ++ userName ++ "!"

personalGreeting : String -> String
personalGreeting userName =
    greetUser "Salut" userName

main =
    text (personalGreeting "Casey")
```

Maintenant, lorsque vous l'exécutez : "Salut, Casey !" Magie ? Non, juste des fonctions qui font leur travail.

## Plongée en profondeur
Autrefois, le code était souvent une longue séquence d'instructions (pensez au code spaghettis). C'était un cauchemar à maintenir. Puis la programmation structurée est apparue, et avec elle, les fonctions. Elm, comme ses prédécesseurs en programmation fonctionnelle, repose fortement sur les fonctions pour l'organisation.

Vous pouvez imbriquer des fonctions, créant des fermetures, ou les garder pures pour simplifier. Elm encourage cette dernière approche : des fonctions pures avec des entrées et sorties bien définies, ce qui facilite le débogage et le test.

Les fonctions Elm peuvent également être d'ordre supérieur, ce qui signifie qu'elles peuvent accepter ou retourner d'autres fonctions. Cela ouvre un monde de composabilité. Cependant, contrairement à certains autres langages, Elm n'a pas de surcharge de fonction ; chaque fonction doit avoir un nom unique.

De plus, Elm impose un système de typage statique strict qui non seulement vérifie les types, mais les infère également, réduisant le code à écrire.

En comparaison avec des alternatives telles que l'organisation de code procédurale ou orientée objet dans d'autres langages, l'approche d'Elm met l'accent sur la simplicité et la prévisibilité. Elm n'a pas d'objets ou de classes. Vous organisez le code avec des fonctions et des modules plutôt qu'avec des classes et des instances.

## Voir aussi
Pour approfondir, consultez ces ressources :
- Le guide officiel d'Elm sur les fonctions : https://guide.elm-lang.org/core_language.html
- La documentation du paquet Elm pour des exemples de fonctions plus complexes : https://package.elm-lang.org/
- Apprenez le système de types d'Elm, qui s'intègre bien avec l'organisation de fonctions : https://elm-lang.org/docs/types
