---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:22:38.698157-07:00
description: "Jak to zrobi\u0107: Elm traktuje obs\u0142ug\u0119 JSON z wyra\u017A\
  no\u015Bci\u0105 i bezpiecze\u0144stwem, g\u0142\xF3wnie za pomoc\u0105 modu\u0142\
  \xF3w `Json.Decode` i `Json.Encode`. Aby rozpocz\u0105\u0107 prac\u0119 z\u2026"
lastmod: '2024-03-13T22:44:35.343069-06:00'
model: gpt-4-0125-preview
summary: "Elm traktuje obs\u0142ug\u0119 JSON z wyra\u017Ano\u015Bci\u0105 i bezpiecze\u0144\
  stwem, g\u0142\xF3wnie za pomoc\u0105 modu\u0142\xF3w `Json.Decode` i `Json.Encode`."
title: Praca z JSON
weight: 38
---

## Jak to zrobić:
Elm traktuje obsługę JSON z wyraźnością i bezpieczeństwem, głównie za pomocą modułów `Json.Decode` i `Json.Encode`. Aby rozpocząć pracę z JSON, musisz najpierw zdefiniować dekoder dla swojego typu danych. Załóżmy, że mamy do czynienia z prostym obiektem profilu użytkownika.

Po pierwsze, zdefiniuj swój typ Elm:

```elm
type alias UserProfile = 
    { id : Int
    , name : String
    , email : String
    }
```

### Dekodowanie JSON do Elm
Aby zdekodować łańcuch JSON do typu `UserProfile`, utwórz dekoder:

```elm
import Json.Decode exposing (Decoder, int, string, field, map3)

userProfileDecoder : Decoder UserProfile
userProfileDecoder =
    map3 UserProfile
        (field "id" int)
        (field "name" string)
        (field "email" string)
```

Aby zdekodować obiekt JSON:

```elm
import Json.Decode exposing (decodeString)

jsonString : String
jsonString = 
    """{"id": 1, "name": "John Doe", "email": "john@example.com"}"""

decoded : Result String UserProfile
decoded =
    decodeString userProfileDecoder jsonString

{- Przykładowe wyjście:
Result.Ok { id = 1, name = "John Doe", email = "john@example.com" }
-}
```

### Kodowanie Elm do JSON
Aby zakodować wartość Elm z powrotem do JSON, skorzystaj z modułu `Json.Encode`.

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
Użycie:
encodeUserProfile { id = 1, name = "John Doe", email = "john@example.com" }

Przykładowe wyjście:
"{"id":1,"name":"John Doe","email":"john@example.com"}"
-}
```

### Biblioteki stron trzecich
Pakiety Elm, takie jak `elm-json-decode-pipeline`, mogą upraszczać tworzenie dekoderów, używając stylu pipeline, co jest szczególnie przydatne przy dekodowaniu złożonych obiektów.

Najpierw dodaj bibliotekę do swojego projektu:

```shell
elm install NoRedInk/elm-json-decode-pipeline
```

Następnie możesz uprościć definicję dekodera w następujący sposób:

```elm
import Json.Decode exposing (int, string, succeed)
import Json.Decode.Pipeline exposing (required, decode)

userProfileDecoder : Decoder UserProfile
userProfileDecoder =
    decode UserProfile
        |> required "id" int
        |> required "name" string
        |> required "email" string

{- Użyj tego dekodera jak wcześniej z decodeString do dekodowania łańcuchów JSON. -}
```

To podejście upraszcza dekoder, czyniąc kod czystszym i bardziej łatwym do utrzymania, szczególnie gdy struktury danych stają się bardziej złożone.
