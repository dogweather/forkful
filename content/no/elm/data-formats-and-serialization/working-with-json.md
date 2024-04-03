---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:22:44.871414-07:00
description: "Hvordan: Elm behandler JSON-h\xE5ndtering med tydelighet og sikkerhet,\
  \ hovedsakelig ved \xE5 bruke modulene `Json.Decode` og `Json.Encode`. For \xE5\
  \ begynne \xE5\u2026"
lastmod: '2024-03-13T22:44:40.729689-06:00'
model: gpt-4-0125-preview
summary: "Elm behandler JSON-h\xE5ndtering med tydelighet og sikkerhet, hovedsakelig\
  \ ved \xE5 bruke modulene `Json.Decode` og `Json.Encode`."
title: Arbeider med JSON
weight: 38
---

## Hvordan:
Elm behandler JSON-håndtering med tydelighet og sikkerhet, hovedsakelig ved å bruke modulene `Json.Decode` og `Json.Encode`. For å begynne å jobbe med JSON, må du først definere en dekoder for datatypen din. La oss anta at vi har med et enkelt brukerprofilobjekt å gjøre.

Definer først din Elm-type:

```elm
type alias UserProfile = 
    { id : Int
    , name : String
    , email : String
    }
```

### Dekoding av JSON til Elm
For å dekode en JSON-streng til `UserProfile`-typen, opprett en dekoder:

```elm
import Json.Decode exposing (Decoder, int, string, field, map3)

userProfileDecoder : Decoder UserProfile
userProfileDecoder =
    map3 UserProfile
        (field "id" int)
        (field "name" string)
        (field "email" string)
```

For å dekode et JSON-objekt:

```elm
import Json.Decode exposing (decodeString)

jsonString : String
jsonString = 
    """{"id": 1, "name": "John Doe", "email": "john@example.com"}"""

decoded : Result String UserProfile
decoded =
    decodeString userProfileDecoder jsonString

{- Eksempel på utdata:
Result.Ok { id = 1, name = "John Doe", email = "john@example.com" }
-}
```

### Koding av Elm til JSON
For å kode en Elm-verdi tilbake til JSON, bruk modulen `Json.Encode`.

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
Bruksmåte:
encodeUserProfile { id = 1, name = "John Doe", email = "john@example.com" }

Eksempel på utdata:
"{"id":1,"name":"John Doe","email":"john@example.com"}"
-}
```

### Tredjepartsbiblioteker
Elm-pakker som `elm-json-decode-pipeline` kan forenkle opprettelsen av dekodere ved å bruke en pipeline-stil, noe som er spesielt nyttig for dekoding av komplekse objekter.

Først, legg til biblioteket i prosjektet ditt:

```shell
elm install NoRedInk/elm-json-decode-pipeline
```

Deretter kan du forenkle definisjonen av dekoderen som følger:

```elm
import Json.Decode exposing (int, string, succeed)
import Json.Decode.Pipeline exposing (required, decode)

userProfileDecoder : Decoder UserProfile
userProfileDecoder =
    decode UserProfile
        |> required "id" int
        |> required "name" string
        |> required "email" string

{- Bruk denne dekoderen som før med decodeString for å dekode JSON-strenger. -}
```

Denne tilnærmingen forenkler dekoderen, noe som gjør koden renere og mer vedlikeholdbar, spesielt etter hvert som datastrukturene blir mer komplekse.
