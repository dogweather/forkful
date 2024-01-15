---
title:                "Envoyer une requête http"
html_title:           "Elm: Envoyer une requête http"
simple_title:         "Envoyer une requête http"
programming_language: "Elm"
category:             "Elm"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/elm/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Pourquoi

Si vous voulez créer une application web ou mobile avec Elm, vous aurez très probablement besoin d'envoyer des requêtes HTTP pour récupérer et envoyer des données depuis et vers un serveur. Cela peut être utile pour intégrer des données en temps réel, créer des formulaires interactifs ou encore se connecter à une API tierce.

## Comment faire

Pour envoyer une requête HTTP en Elm, vous aurez besoin de trois éléments principaux : une URL, une méthode (GET, POST, PUT, etc.) et éventuellement des données à envoyer (paramètres, corps de la requête, etc.). Voici un exemple de code pour une requête GET :

```
port module Main exposing (..)

import Html exposing (text)
import Http

type Msg = FetchSuccess (Result Http.Error String)

fetchData : Cmd Msg
fetchData =
  Http.getString "https://monsite.com/api/data"
    |> Http.send FetchSuccess

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    FetchSuccess result ->
      case result of
        Ok data ->
          ( model, Cmd.none )
        Err error ->
          ( model, Cmd.none )
    ...

view : Model -> Html Msg
view model =
  div []
    [ button [ onClick fetchData ] [ text "Fetch Data" ]
    , ...
    ]

main : Program Nothing
main =
  Browser.element
    { init = init
    , update = update
    , view = view
    , subscriptions = always Sub.none
    }
```

Ce code crée une fonction `fetchData` qui envoie une requête GET à l'URL spécifiée et renvoie le résultat dans un message `FetchSuccess`. Dans la fonction `update`, nous pouvons gérer le résultat de la requête en utilisant `case` pour gérer les différentes possibilités (succès ou échec). Enfin, dans la fonction `view`, nous créons un bouton qui, lorsque cliqué, enverra la requête avec la fonction `fetchData` définie précédemment.

## Plongée en profondeur

En plus des fonctions de requête HTTP de base, Elm offre également des packages tels que [elm/http-builder](https://package.elm-lang.org/packages/elm/http-builder/latest/) pour créer des requêtes plus complexes avec des en-têtes personnalisés, des timeouts et plus encore.

De plus, certaines applications Elm peuvent nécessiter l'utilisation de cookies pour des raisons d'authentification ou de sessions. Dans ce cas, vous pouvez utiliser [elm/browser](https://package.elm-lang.org/packages/elm/browser/latest/) pour gérer les cookies lors de l'envoi de requêtes HTTP.

## Voir aussi

- Guide officiel pour les requêtes HTTP en Elm : https://guide.elm-lang.org/effects/http.html
- Exemples de requêtes HTTP en Elm : https://github.com/dwyl/learn-elm/blob/master/http-requests/README.md
- Documentation du package elm/http-builder : https://package.elm-lang.org/packages/elm/http-builder/latest/