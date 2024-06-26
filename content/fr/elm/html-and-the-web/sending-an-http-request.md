---
date: 2024-01-20 17:59:20.915027-07:00
description: "How to: (Comment faire :) Historiquement, Elm a rendu les requ\xEAtes\
  \ HTTP plus s\xFBres en utilisant The Elm Architecture, qui g\xE8re les effets de\
  \ bord de fa\xE7on\u2026"
lastmod: '2024-04-05T22:51:11.698741-06:00'
model: gpt-4-1106-preview
summary: "(Comment faire :) Historiquement, Elm a rendu les requ\xEAtes HTTP plus\
  \ s\xFBres en utilisant The Elm Architecture, qui g\xE8re les effets de bord de\
  \ fa\xE7on pr\xE9visible."
title: "Envoi d'une requ\xEAte HTTP"
weight: 44
---

## How to: (Comment faire :)
```Elm
import Http
import Json.Decode as Decode

type alias User =
    { id : Int
    , name : String
    }

userDecoder : Decode.Decoder User
userDecoder =
    Decode.map2 User
        (Decode.field "id" Decode.int)
        (Decode.field "name" Decode.string)

fetchUser : Int -> Cmd Msg
fetchUser userId =
    Http.get
        { url = "https://api.example.com/users/" ++ String.fromInt(userId)
        , decoder = userDecoder
        }
        |> Http.send UserFetched

type Msg
    = UserFetched (Result Http.Error User)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        UserFetched (Ok user) ->
            ({ model | user = Just user }, Cmd.none)

        UserFetched (Err _) ->
            (model, Cmd.none)
```

## Deep Dive (Plongée en profondeur)
Historiquement, Elm a rendu les requêtes HTTP plus sûres en utilisant The Elm Architecture, qui gère les effets de bord de façon prévisible. Côté alternatives, on pourrait regarder vers des packages comme `elm-http-builder` pour une plus grande flexibilité. En coulisses, Elm utilise des commandes pour déclencher des requêtes HTTP, évitant ainsi les effets de bord aléatoires et garantissant un flux de données unidirectionnel.

## See Also (Voir aussi)
- Documentation officielle sur HTTP en Elm : [Elm HTTP](https://package.elm-lang.org/packages/elm/http/latest/)
- JSON Decoder : [Elm JSON Decode Pipeline](https://package.elm-lang.org/packages/NoRedInk/elm-json-decode-pipeline/latest)
- Handling HTTP in Elm : [Elm Guide - HTTP](https://guide.elm-lang.org/effects/http.html)
