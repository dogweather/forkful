---
title:                "Arbeta med JSON"
aliases:
- /sv/elm/working-with-json/
date:                  2024-02-03T19:22:49.179976-07:00
model:                 gpt-4-0125-preview
simple_title:         "Arbeta med JSON"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/elm/working-with-json.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Vad & Varför?
Att arbeta med JSON i Elm handlar om att avkoda JSON-data till Elm-typer och koda om Elm-värden tillbaka till JSON. Denna process är avgörande för webbapplikationer för att interagera med API:er och externa datakällor, vilket möjliggör ett sömlöst utbyte av data mellan klienten (Elm) och servrar eller andra tjänster.

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
