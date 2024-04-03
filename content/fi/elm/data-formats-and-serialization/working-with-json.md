---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:22:49.311491-07:00
description: "JSON:n k\xE4sittely Elm:ss\xE4 tarkoittaa JSON-datan dekoodaamista Elm-tyypeiksi\
  \ ja Elm-arvojen enkoodaamista takaisin JSON:ksi. T\xE4m\xE4 prosessi on ratkaisevan\u2026"
lastmod: '2024-03-13T22:44:56.510403-06:00'
model: gpt-4-0125-preview
summary: "JSON:n k\xE4sittely Elm:ss\xE4 tarkoittaa JSON-datan dekoodaamista Elm-tyypeiksi\
  \ ja Elm-arvojen enkoodaamista takaisin JSON:ksi."
title: "Ty\xF6skentely JSON:n kanssa"
weight: 38
---

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
