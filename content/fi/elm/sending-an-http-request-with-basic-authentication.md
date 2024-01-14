---
title:                "Elm: Perusautentikaatiolla http-pyynnön lähettäminen"
simple_title:         "Perusautentikaatiolla http-pyynnön lähettäminen"
programming_language: "Elm"
category:             "Elm"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/elm/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Miksi
Elm ohjelmointikieli tarjoaa helpon tavan luoda web-sovelluksia, jotka käyttävät HTTP-pyyntöjä. Tässä artikkelissa kerron, kuinka voit lähettää HTTP-pyynnön perusautentikoinnilla käyttäen Elmia.

## Kuinka tehdä
Lähetä HTTP-pyyntö perusautentikoinnilla Elmissa käyttäen "Http.sendWithCredentials" -funktiota ja anna pyynnön mukana tarvittavat autentikointitiedot.

```Elm
import Http
import Json.Decode exposing (int, string, decodeString)

type alias Response = {
    status: Int,
    message: String
}

sendRequest : Cmd Msg
sendRequest =
    Http.sendWithCredentials
        { method = "GET"
        , headers = []
        , url = "https://example.com/api/"
        , body = Http.stringBody ""
        , expect = Http.expectJson decodeResponse
        }

decodeResponse : Decoder Response
decodeResponse =
    Decode.map2 Response
        ("status" := int)
        ("message" := string)
```

## Syvemmälle
Perusautentikointi on yleinen tapa suojata web-sovelluksia käyttäen käyttäjätunnusta ja salasanaa. Se vaatii, että jokainen HTTP-pyyntö sisältää autentikointitiedot.

Perusautentikointi voidaan toteuttaa käyttäen "Authorization" -otsaketta, johon liitetään käyttäjätunnus ja salasana, jotka on koodattu Base64-muotoon. Elm tarjoaa "Http.stringBody" -funktion, jolla voit lisätä autentikointitiedot pyyntöön.

## Katso myös
- Elm HTTP -dokumentaatio: https://package.elm-lang.org/packages/elm/http/latest/
- Base64-koodaus Elmissa: https://package.elm-lang.org/packages/truqu/elm-base64/latest/