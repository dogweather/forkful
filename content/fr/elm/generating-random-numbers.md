---
title:                "Elm: Création de nombres aléatoires"
simple_title:         "Création de nombres aléatoires"
programming_language: "Elm"
category:             "Elm"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/elm/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Pourquoi S'engager Dans La Génération De Nombres Aléatoires?

La génération de nombres aléatoires est un aspect essentiel de la programmation. Elle permet de créer des jeux, des simulations et bien plus encore. En utilisant Elm, il est possible de générer des nombres aléatoires de manière facile et efficace. Dans cet article, nous allons voir comment faire cela.

## Comment Le Faire En Elm

Pour générer des nombres aléatoires en Elm, nous allons utiliser la bibliothèque `Random` intégrée. Voici un exemple de code:

```
Elm
import Random

-- Génère un nombre aléatoire entre 1 et 10
randomNumber : Int
randomNumber =
    Random.int 1 10

main =
    Html.text (toString randomNumber)
```

Lorsque nous exécutons ce code, nous pouvons obtenir différentes valeurs pour `randomNumber`. Par exemple, cela peut être `4`, `8` ou `2`. Il est également possible de générer des nombres aléatoires d'autres types, tels que `Bool` (boolean) ou `Float` (flottant).

Vous pouvez également utiliser des générateurs personnalisés en combinant des générateurs de base avec les fonctions `map` et `andThen`. Voici un autre exemple de code:

```
Elm
import Random

-- Génère une couleur aléatoire en utilisant des nombres aléatoires RGB
randomColor : String
randomColor =
    Random.color
    |> Random.map (\(r, g, b) -> ("rgb(" ++ toString r ++ "," ++ toString g ++ "," ++ toString b ++ ")"))
    |> Random.andThen identity

main =
    Html.text randomColor
```

Ce code générera une couleur au format RGB telle que `"rgb(255,153,204)"`. Les possibilités sont infinies lorsque vous utilisez la bibliothèque `Random` en combinaison avec d'autres fonctions dans Elm.

## Plongée En Profondeur

La bibliothèque `Random` en Elm suit le paradigme fonctionnel pur, ce qui la rend prévisible et facile à tester. De plus, elle utilise un générateur pseudo-aléatoire qui peut être initialisé avec une graine (seed), ce qui permet de reproduire la même série de nombres aléatoires. Cette graine peut être définie en utilisant la fonction `initialSeed` de la bibliothèque `Seed`.

De plus, la bibliothèque `Random` propose également des fonctions pour générer des nombres aléatoires distribués de manière uniforme, tels que `uniformInt` et `uniformFloat`. Cela peut être utile pour des simulations ou des algorithmes de machine learning.

## Voir Aussi

- [Documentation de la bibliothèque `Random` en Elm](https://package.elm-lang.org/packages/elm/random/latest/)
- [Exemples de code pour générer des nombres aléatoires en Elm](https://www.hellorust.com/demos/elm-random/index.html)