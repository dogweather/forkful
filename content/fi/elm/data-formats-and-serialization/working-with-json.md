---
title:                "Työskentely JSON:n kanssa"
aliases: - /fi/elm/working-with-json.md
date:                  2024-02-03T19:22:49.311491-07:00
model:                 gpt-4-0125-preview
simple_title:         "Työskentely JSON:n kanssa"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/elm/working-with-json.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Mikä & Miksi?
JSON:n käsittely Elm:ssä tarkoittaa JSON-datan dekoodaamista Elm-tyypeiksi ja Elm-arvojen enkoodaamista takaisin JSON:ksi. Tämä prosessi on ratkaisevan tärkeä web-sovelluksille vuorovaikutuksessa API:en ja ulkoisten tietolähteiden kanssa, mahdollistaen saumattoman datan vaihdon asiakkaan (Elm) ja palvelimien tai muiden palveluiden välillä.

## Kuinka:

Elm käsittelee JSON:ia selkeästi ja turvallisesti, pääasiassa käyttäen `Json.Decode`- ja `Json.Encode`-moduleita. Aloittaaksesi JSON:n käsittelyn, sinun täytyy ensin määritellä dekooderi datatyypillesi. Oletetaan, että käsittelemme yksinkertaista käyttäjäprofiilin objektia.

Määrittele ensin Elm-tyyppisi:

```elm
type alias UserProfile = 
    { id : Int
    , name : String
    , email : String
    }
```

### JSON:n dekoodaus Elm:ksi

Dekoodataksesi JSON merkkijonon `UserProfile`-tyypiksi, luo dekooderi:

```elm
import Json.Decode exposing (Decoder, int, string, field, map3)

userProfileDecoder : Decoder UserProfile
userProfileDecoder =
    map3 UserProfile
        (field "id" int)
        (field "name" string)
        (field "email" string)
```

JSON-objektin dekoodaamiseksi:

```elm
import Json.Decode exposing (decodeString)

jsonString : String
jsonString = 
    """{"id": 1, "name": "John Doe", "email": "john@example.com"}"""

decoded : Result String UserProfile
decoded =
    decodeString userProfileDecoder jsonString

{- Näyte tuloste:
Result.Ok { id = 1, name = "John Doe", email = "john@example.com" }
-}
```

### Elm:n enkoodaus JSON:ksi

Enkoodataksesi Elm-arvon takaisin JSON:ksi, hyödynnä `Json.Encode`-moduulia.

```elm
import Json.Encode exposing (object, int, string)

encodeUserProfile : UserProfile -> String
encodeUserProfile userProfile =
    object
        [ ("id", int userProfile.id)
        , ("name", string userProfile.name)
        , ("email", string userProfile.email)
        ]
        |> Json.Encode.encode 0

{- 
Käyttö:
encodeUserProfile { id = 1, name = "John Doe", email = "john@example.com" }

Näyte tuloste:
"{"id":1,"name":"John Doe","email":"john@example.com"}"
-}
```

### Kolmannen osapuolen kirjastot

Elm-paketit kuten `elm-json-decode-pipeline` voivat yksinkertaistaa dekooderien luomista käyttämällä putkityyliä, mikä on erityisen kätevää monimutkaisten objektien dekoodauksessa.

Lisää ensin kirjasto projektiisi:

```shell
elm install NoRedInk/elm-json-decode-pipeline
```

Sen jälkeen voit yksinkertaistaa dekooderin määritelmää seuraavasti:

```elm
import Json.Decode exposing (int, string, succeed)
import Json.Decode.Pipeline exposing (required, decode)

userProfileDecoder : Decoder UserProfile
userProfileDecoder =
    decode UserProfile
        |> required "id" int
        |> required "name" string
        |> required "email" string

{- Käytä tätä dekooderia kuten aiemmin decodeString-toiminnolla dekoodaamaan JSON-merkkijonot. -}
```

Tämä lähestymistapa yksinkertaistaa dekooderia, tekee koodista selkeämmän ja ylläpidettävämmän, erityisesti kun data-rakenteet monimutkaistuvat.
