---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:22:38.698157-07:00
description: "Praca z JSON w Elm polega na dekodowaniu danych JSON do typ\xF3w Elm\
  \ i kodowaniu warto\u015Bci Elm z powrotem na JSON. Proces ten jest kluczowy dla\
  \ aplikacji\u2026"
lastmod: 2024-02-19 22:04:54.474064
model: gpt-4-0125-preview
summary: "Praca z JSON w Elm polega na dekodowaniu danych JSON do typ\xF3w Elm i kodowaniu\
  \ warto\u015Bci Elm z powrotem na JSON. Proces ten jest kluczowy dla aplikacji\u2026"
title: Praca z JSON
---

{{< edit_this_page >}}

## Co i dlaczego?
Praca z JSON w Elm polega na dekodowaniu danych JSON do typów Elm i kodowaniu wartości Elm z powrotem na JSON. Proces ten jest kluczowy dla aplikacji internetowych do interakcji z API i zewnętrznymi źródłami danych, umożliwiając płynną wymianę danych między klientem (Elm) a serwerami lub innymi usługami.

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
