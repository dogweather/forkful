---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:22:44.871414-07:00
description: "\xC5 jobbe med JSON i Elm handler om \xE5 dekode JSON-data til Elm-typer\
  \ og \xE5 kode Elm-verdier tilbake til JSON. Denne prosessen er avgj\xF8rende for\u2026"
lastmod: '2024-03-11T00:14:14.277270-06:00'
model: gpt-4-0125-preview
summary: "\xC5 jobbe med JSON i Elm handler om \xE5 dekode JSON-data til Elm-typer\
  \ og \xE5 kode Elm-verdier tilbake til JSON. Denne prosessen er avgj\xF8rende for\u2026"
title: Arbeider med JSON
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Å jobbe med JSON i Elm handler om å dekode JSON-data til Elm-typer og å kode Elm-verdier tilbake til JSON. Denne prosessen er avgjørende for webapplikasjoner for å samhandle med APIer og eksterne datakilder, noe som tillater en sømløs utveksling av data mellom klienten (Elm) og servere eller andre tjenester.

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
