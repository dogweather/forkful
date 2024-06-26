---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:07:43.888978-07:00
description: "Hoe te: Ok\xE9, tijd voor code. Elm maakt HTTP-verzoeken met behulp\
  \ van de `Http` module. Hier is een snel voorbeeld om wat JSON op te halen."
lastmod: '2024-03-13T22:44:50.721540-06:00'
model: gpt-4-0125-preview
summary: "Ok\xE9, tijd voor code."
title: Een HTTP-verzoek verzenden
weight: 44
---

## Hoe te:
Oké, tijd voor code. Elm maakt HTTP-verzoeken met behulp van de `Http` module. Hier is een snel voorbeeld om wat JSON op te halen:

```Elm
import Http
import Json.Decode as Decode

type alias User =
    { id : Int
    , username : String
    }

userDecoder : Decode.Decoder User
userDecoder =
    Decode.map2 User
        (Decode.field "id" Decode.int)
        (Decode.field "username" Decode.string)

fetchUser : Cmd Msg
fetchUser =
    Http.get
        { url = "https://api.example.com/user/1"
        , decoder = userDecoder
        }
        |> Http.send UserFetched

type Msg
    = UserFetched (Result Http.Error User)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    geval msg van
        UserFetched (Ok user) ->
            ({ model | user = Just user }, Cmd.none)

        UserFetched (Err _) ->
            (model, Cmd.none)
```

Voorbeeldoutput wanneer `UserFetched` een `Ok user` is:

```Elm
{ id = 1, username = "ElmerFudd" }
```

## Diepgaande duik
Het verzenden van HTTP-verzoeken is niet nieuw; het is de ruggengraat van webcommunicatie sinds de jaren 90. Elm verpakt de complexiteit in de gebruiksvriendelijke `Http` module, met de nadruk op veiligheid en eenvoud. In tegenstelling tot de vroege dagen, abstraheert Elm de rommelige delen zoals XMLHttprequest en JSON-parsing weg. Alternatieven zoals het direct gebruiken van JavaScripts Fetch API of XMLHttpRequest zijn mogelijk met poorten, maar Elm's ingebouwde manier houdt je code type-veilig en puur. Het behandelt bijeffecten via zijn krachtige architectuur zonder de betrouwbaarheid van je app in gevaar te brengen.

## Zie Ook
Voor meer gedetailleerde uitleg en probleemoplossing, bekijk deze bronnen:

- Elm package documentatie voor HTTP: [https://package.elm-lang.org/packages/elm/http/latest/](https://package.elm-lang.org/packages/elm/http/latest/)
- JSON Decoderen in Elm: [https://package.elm-lang.org/packages/elm/json/latest/](https://package.elm-lang.org/packages/elm/json/latest/)
- Elm Gids over HTTP-verzoeken: [https://guide.elm-lang.org/effects/http.html](https://guide.elm-lang.org/effects/http.html)
- Elm Discuss voor inzichten van de gemeenschap: [https://discourse.elm-lang.org/](https://discourse.elm-lang.org/)
