---
date: 2024-01-20 17:59:23.247905-07:00
description: "How to: (Kuinka tehd\xE4:) ."
lastmod: '2024-03-13T22:44:56.485419-06:00'
model: gpt-4-1106-preview
summary: .
title: "HTTP-pyynn\xF6n l\xE4hett\xE4minen"
weight: 44
---

## How to: (Kuinka tehdä:)
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

getUser : Cmd Msg
getUser =
  Http.get
    { url = "https://api.example.com/users/1"
    , decoder = userDecoder
    }
```
Kun suoritat tämän, saat käyttäjän tiedot, jos kaikki menee hyvin: `{ id = 1, name = "Alice" }`.

## Deep Dive (Syväsukellus)
HTTP-pyynnöt ovat nettisovellusten selkäranka. Historiassa käytettiin pääasiassa vain XMLHttpReqest-objektia, mutta Elm tarjoaa yksinkertaistetun HTTP-moduulin, joka kapseloi monimutkaisuuden ja keskittyy puhtaaseen toiminnallisuuteen. Elmissä kaikki HTTP-pyynnöt käsitellään komentoina (Cmd), jotka seuraavat sovelluksen tilan puhtaiden päivitysten mallia. Vaihtoehtoisesti voit käyttää WebSocketsia reaaliaikaiseen viestintään. Elm 0.19 version myötä, Json.Decode-moduulin käyttö on tehty helpommaksi tietojen jäsentämiseksi.

## See Also (Katso Myös)
- Elm HTTP package documentation: [https://package.elm-lang.org/packages/elm/http/latest/](https://package.elm-lang.org/packages/elm/http/latest/)
- JSON Decoding in Elm: [https://guide.elm-lang.org/effects/json.html](https://guide.elm-lang.org/effects/json.html)
- Elm Lang Official Guide (HTTP Requests): [https://guide.elm-lang.org/effects/http.html](https://guide.elm-lang.org/effects/http.html)
