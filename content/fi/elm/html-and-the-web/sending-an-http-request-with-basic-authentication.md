---
date: 2024-01-20 18:01:40.251626-07:00
description: "How to: | Kuinka: Perusautentikointi (Basic Authentication) oli yksi\
  \ ensimm\xE4isist\xE4 verkkopalvelujen tunnistusmenetelmist\xE4. Yksinkertaisuutensa\
  \ vuoksi se\u2026"
lastmod: '2024-04-05T22:38:57.091371-06:00'
model: gpt-4-1106-preview
summary: "| Kuinka: Perusautentikointi (Basic Authentication) oli yksi ensimm\xE4\
  isist\xE4 verkkopalvelujen tunnistusmenetelmist\xE4. Yksinkertaisuutensa vuoksi\
  \ se on edelleen k\xE4yt\xF6ss\xE4, mutta huomaa, ett\xE4 se ei ole turvallisin\
  \ vaihtoehto ilman HTTPS-yhteytt\xE4, sill\xE4 tunnukset l\xE4hetet\xE4\xE4n base64-koodattuina\
  \ selkotekstin\xE4. Vaihtoehtoja perusautentikoinnille ovat Bearer-autentikointi,\
  \ OAuth ja API-avaimet. N\xE4iss\xE4 metodeissa tietoturvallisuus on usein parempi.\
  \ Kun l\xE4het\xE4mme perusautentikointia vaativan pyynn\xF6n Elm:ss\xE4, meid\xE4\
  n t\xE4ytyy lis\xE4t\xE4 `Authorization` otsikko sis\xE4lt\xE4en base64-koodatut\
  \ tunnukset HTTP-pyynn\xF6n otsikoihin. Elm k\xE4ytt\xE4\xE4 moduulia `Http` HTTP-pyynt\xF6\
  jen k\xE4sittelyyn ja `Base64` moduulia tunnusten koodaamiseen."
title: "HTTP-pyynn\xF6n l\xE4hett\xE4minen perusautentikoinnilla"
weight: 45
---

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
