---
title:                "HTTP-pyynnön lähettäminen perusautentikoinnilla"
date:                  2024-01-20T18:01:40.251626-07:00
model:                 gpt-4-1106-preview
simple_title:         "HTTP-pyynnön lähettäminen perusautentikoinnilla"
programming_language: "Elm"
category:             "Elm"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/elm/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## What & Why? | Mikä & Miksi?
Perusautentikoinnilla varustetun HTTP-pyynnön lähettäminen on prosessi, jossa käyttäjätunnus ja salasana lähetetään HTTP-otsikoissa palvelimelle. Käytämme tätä menetelmää, koska se on yksinkertainen tapa varmistaa käyttäjän identiteetti ennen palvelun käyttöoikeuksien myöntämistä.

## How to: | Kuinka:
```Elm
import Http
import Base64

type alias MyModel = { ... }

type Msg = ...
    | RequestFailed Http.Error

-- ENCODE CREDENTIALS
encodeCredentials : String -> String -> String
encodeCredentials username password =
    "Basic " ++ Base64.encode (username ++ ":" ++ password)

-- SEND REQUEST WITH BASIC AUTH
sendRequest : String -> String -> Cmd Msg
sendRequest username password =
    let
        headers =
            [ Http.header "Authorization" (encodeCredentials username password) ]
    in
    Http.get
        { url = "https://your-api.com/data"
        , expect = Http.expectJson ...
        }
        |> Http.withHeaders headers
        |> Http.send RequestFailed

-- SAMPLE OUTPUT (Not shown in Elm, based on Msg and update function logic)
```

## Deep Dive | Syväsukellus:
Perusautentikointi (Basic Authentication) oli yksi ensimmäisistä verkkopalvelujen tunnistusmenetelmistä. Yksinkertaisuutensa vuoksi se on edelleen käytössä, mutta huomaa, että se ei ole turvallisin vaihtoehto ilman HTTPS-yhteyttä, sillä tunnukset lähetetään base64-koodattuina selkotekstinä.

Vaihtoehtoja perusautentikoinnille ovat Bearer-autentikointi, OAuth ja API-avaimet. Näissä metodeissa tietoturvallisuus on usein parempi.

Kun lähetämme perusautentikointia vaativan pyynnön Elm:ssä, meidän täytyy lisätä `Authorization` otsikko sisältäen base64-koodatut tunnukset HTTP-pyynnön otsikoihin. Elm käyttää moduulia `Http` HTTP-pyyntöjen käsittelyyn ja `Base64` moduulia tunnusten koodaamiseen.

## See Also | Katso Myös:
- Elm's official HTTP package documentation: https://package.elm-lang.org/packages/elm/http/latest/
- Base64 encoding with Elm: https://package.elm-lang.org/packages/truqu/elm-base64/latest/
- More on HTTP Basic Authentication: https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication
- Secure Elm applications: https://elm-lang.org/news/security-in-elm
