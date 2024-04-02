---
changelog:
- 2024-02-27, dogweather, edited and tested
- 2024-02-27, gpt-4-0125-preview, translated from English
date: 2024-02-27 22:50:36.803137-07:00
description: "G\xE9n\xE9rer des nombres al\xE9atoires en Elm n\xE9cessite l'utilisation\
  \ du module `Random` pour produire des nombres pseudo-al\xE9atoires, qui s'av\xE8\
  rent utiles pour\u2026"
lastmod: '2024-03-13T22:44:57.684496-06:00'
model: gpt-4-0125-preview
summary: "G\xE9n\xE9rer des nombres al\xE9atoires en Elm n\xE9cessite l'utilisation\
  \ du module `Random` pour produire des nombres pseudo-al\xE9atoires, qui s'av\xE8\
  rent utiles pour\u2026"
title: "G\xE9n\xE9ration de nombres al\xE9atoires"
weight: 12
---

## Quoi & Pourquoi ?
Générer des nombres aléatoires en Elm nécessite l'utilisation du module `Random` pour produire des nombres pseudo-aléatoires, qui s'avèrent utiles pour une variété de tâches telles que les jeux, les simulations, et même en tant que partie d'algorithmes nécessitant des processus stochastiques. Cette capacité permet aux développeurs d'ajouter de l'imprévisibilité et de la variété à leurs applications, améliorant l'expérience utilisateur et la fonctionnalité.

## Comment faire :
La nature purement fonctionnelle d'Elm signifie que vous ne pouvez pas générer des nombres aléatoires directement comme vous pourriez le faire dans les langages impératifs. Au lieu de cela, vous utilisez le module `Random` conjointement avec des commandes. Voici un exemple simple qui génère un entier aléatoire entre 1 et 100.

D'abord, installez le module `Random` avec `elm install elm/random`. Ensuite, importez-le dans votre fichier Elm, ainsi que les modules HTML et événementiels nécessaires, comme ceci :

`src/Main.elm`

```elm
module Main exposing (..)

import Browser
import Html exposing (Html, button, text, div)
import Html.Events exposing (onClick)
import Random
```

Pour que cet exemple soit autonome, vous pouvez ajouter ce code de base :
```elm
main =
  Browser.element { init = init, update = update, subscriptions = subscriptions, view = view }

init : () -> (Model, Cmd Msg)
init _ =
  (Model 0, Cmd.none)

subscriptions : Model -> Sub Msg
subscriptions _ =
  Sub.none
```

Ensuite, définissez une **commande** pour générer un nombre aléatoire. Cela implique de mettre en place un type `Msg` pour gérer le nombre aléatoire une fois qu'il est généré, un `Model` pour le stocker, et une fonction de mise à jour pour tout lier.
```elm
type Msg
    = Generate
    | NewRandom Int

type alias Model = { randomNumber : Int }

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        Generate ->
            ( model, Random.generate NewRandom (Random.int 1 100) )

        NewRandom number ->
            ( { model | randomNumber = number }, Cmd.none )
```

Pour déclencher la génération d'un nombre, vous pourriez envoyer un message `Generate`, par exemple, via un bouton dans votre vue :
```elm
view : Model -> Html Msg
view model =
    div []
        [ div [] [ text ("Nombre Aléatoire : " ++ String.fromInt(model.randomNumber)) ]
        , button [ onClick Generate ] [ text "Générer" ]
        ]
```

Lorsque vous cliquez sur le bouton "Générer", un nombre aléatoire entre 1 et 100 sera affiché.

Cette approche simpliste peut être adaptée et élargie, en tirant parti des autres fonctions du module `Random` pour produire des flottants aléatoires, des listes, ou même des structures de données complexes basées sur des types personnalisés, offrant un vaste terrain de jeu pour ajouter de l’imprévisibilité à vos applications Elm.

Le Guide Elm va bien plus en détail. Il a aussi [un exemple pour lancer un dé à six faces](https://guide.elm-lang.org/effects/random).
