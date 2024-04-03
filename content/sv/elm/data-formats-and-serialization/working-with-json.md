---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:22:49.179976-07:00
description: "Hur man g\xF6r: Elm behandlar JSON-hantering med explicithet och s\xE4\
  kerhet, fr\xE4mst genom att anv\xE4nda modulerna `Json.Decode` och `Json.Encode`.\
  \ F\xF6r att b\xF6rja\u2026"
lastmod: '2024-03-13T22:44:37.850937-06:00'
model: gpt-4-0125-preview
summary: "Elm behandlar JSON-hantering med explicithet och s\xE4kerhet, fr\xE4mst\
  \ genom att anv\xE4nda modulerna `Json.Decode` och `Json.Encode`."
title: Arbeta med JSON
weight: 38
---

## Hur man gör:
Elm behandlar JSON-hantering med explicithet och säkerhet, främst genom att använda modulerna `Json.Decode` och `Json.Encode`. För att börja arbeta med JSON måste du först definiera en avkodare för din datatyp. Låt oss anta att vi har att göra med ett enkelt användarprofilobjekt.

Först, definiera din Elm-typ:

```elm
type alias UserProfile = 
    { id : Int
    , name : String
    , email : String
    }
```

### Avkoda JSON till Elm
För att avkoda en JSON-sträng till typen `UserProfile`, skapa en avkodare:

```elm
import Json.Decode exposing (Decoder, int, string, field, map3)

userProfileDecoder : Decoder UserProfile
userProfileDecoder =
    map3 UserProfile
        (field "id" int)
        (field "name" string)
        (field "email" string)
```

För att avkoda ett JSON-objekt:

```elm
import Json.Decode exposing (decodeString)

jsonString : String
jsonString = 
    """{"id": 1, "name": "John Doe", "email": "john@example.com"}"""

decoded : Result String UserProfile
decoded =
    decodeString userProfileDecoder jsonString

{- Exempelutdata:
Result.Ok { id = 1, name = "John Doe", email = "john@example.com" }
-}
```

### Koda Elm till JSON
För att koda om ett Elm-värde tillbaka till JSON, använd `Json.Encode`-modulen.

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
Användning:
encodeUserProfile { id = 1, name = "John Doe", email = "john@example.com" }

Exempelutdata:
"{"id":1,"name":"John Doe","email":"john@example.com"}"
-}
```

### Tredjepartsbibliotek
Elm-paket som `elm-json-decode-pipeline` kan förenkla skapandet av avkodare genom att använda en pipeline-stil, vilket är särskilt praktiskt för avkodning av komplexa objekt.

Först, lägg till biblioteket i ditt projekt:

```shell
elm install NoRedInk/elm-json-decode-pipeline
```

Sedan kan du förenkla definitionen av avkodaren på följande sätt:

```elm
import Json.Decode exposing (int, string, succeed)
import Json.Decode.Pipeline exposing (required, decode)

userProfileDecoder : Decoder UserProfile
userProfileDecoder =
    decode UserProfile
        |> required "id" int
        |> required "name" string
        |> required "email" string

{- Använd denna avkodare som tidigare med decodeString för att avkoda JSON-strängar. -}
```

Detta tillvägagångssätt förenklar avkodaren, vilket gör koden renare och mer underhållbar, särskilt när datatrukturerna blir mer komplexa.
