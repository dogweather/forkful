---
title:                "Génération de nombres aléatoires"
date:                  2024-01-20T17:48:50.715190-07:00
model:                 gpt-4-1106-preview
simple_title:         "Génération de nombres aléatoires"
programming_language: "Elm"
category:             "Elm"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/elm/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Quoi et Pourquoi ?
Générer des nombres aléatoires, c'est créer des nombres imprévisibles. Les programmeurs utilisent cette méthode pour tout, des jeux vidéo aux simulations, en passant par le choix aléatoire dans les tests logiciels.

## Comment faire :
```Elm
import Random

-- Générez un nombre aléatoire entre 1 et 100
randomInt : Random.Generator Int
randomInt = Random.int 1 100

-- Comment utiliser le générateur dans un programme Elm
main =
    Random.generate NewRandomNumber randomInt
        |> Html.map (\_ -> Html.text "")
```
Ce code initialise un générateur (`randomInt`) et utilise `Random.generate` dans `main` pour créer des nombres aléatoires.

## Exploration en Profondeur
Historiquement, créer de vrais nombres aléatoires en informatique est complexe, car les ordinateurs sont conçus pour être prédictifs. En Elm, on utilise une simulation de l'aléatoire avec les générateurs pseudos-aléatoires. Cela signifie que, connaissant la graine (seed), les résultats seront toujours les mêmes.

Elm gère l'aléatoire différemment des langages impératifs. Ici, on manipule des générateurs au sein d'une architecture plus purement fonctionnelle qui prend soin de l'état de l'application. Cela garantit la prévisibilité de l'application tout en utilisant des données qui semblent aléatoires.

Les alternatives incluent l'utilisation de générateurs de nombres aléatoires sur le serveur ou l'intégration d'API tierces qui fournissent de l'aléatoire plus complexe, comme la cryptographie.

## Voir Aussi
- Documentation Elm sur les nombres aléatoires: [Elm Random](https://package.elm-lang.org/packages/elm/random/latest/)
- Une introduction à l'aléatoire en informatique: [Random.org](https://www.random.org/randomness/)
- Un guide pour comprendre la programmation fonctionnelle: [Learn You a Haskell for Great Good!](http://learnyouahaskell.com/) (Bien que pour Haskell, les concepts sont transférables à Elm)
