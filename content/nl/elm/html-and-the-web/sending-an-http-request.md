---
title:                "Een HTTP-verzoek verzenden"
aliases:
- /nl/elm/sending-an-http-request.md
date:                  2024-01-28T22:07:43.888978-07:00
model:                 gpt-4-0125-preview
simple_title:         "Een HTTP-verzoek verzenden"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/elm/sending-an-http-request.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?

In Elm is het verzenden van een HTTP-verzoek de manier waarop je app communiceert met andere webservices om gegevens uit te wisselen. Programmeurs doen dit om informatie van of naar servers te halen of te versturen, waarmee de dynamiek van de app wordt gevoed, zoals gebruikersaccounts, scores of nieuwsupdates.

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
