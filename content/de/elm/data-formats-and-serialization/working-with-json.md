---
title:                "Arbeiten mit JSON"
aliases:
- de/elm/working-with-json.md
date:                  2024-02-03T19:22:57.583005-07:00
model:                 gpt-4-0125-preview
simple_title:         "Arbeiten mit JSON"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/elm/working-with-json.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Was & Warum?
Die Arbeit mit JSON in Elm dreht sich um das Dekodieren von JSON-Daten in Elm-Typen und das Kodieren von Elm-Werten zurück in JSON. Dieser Prozess ist entscheidend, damit Webanwendungen mit APIs und externen Datenquellen interagieren können, was einen nahtlosen Datenaustausch zwischen dem Client (Elm) und Servern oder anderen Diensten ermöglicht.

## Wie geht das:

Elm behandelt die JSON-Verarbeitung mit Explizitheit und Sicherheit, hauptsächlich unter Verwendung der Module `Json.Decode` und `Json.Encode`. Um mit JSON zu beginnen, müssen Sie zuerst einen Decoder für Ihren Datentyp definieren. Nehmen wir an, wir haben es mit einem einfachen Benutzerprofilobjekt zu tun.

Definieren Sie zunächst Ihren Elm-Typ:

```elm
type alias UserProfile = 
    { id : Int
    , name : String
    , email : String
    }
```

### JSON in Elm dekodieren

Um eine JSON-Zeichenkette in den Typ `UserProfile` zu dekodieren, erstellen Sie einen Decoder:

```elm
import Json.Decode exposing (Decoder, int, string, field, map3)

userProfileDecoder : Decoder UserProfile
userProfileDecoder =
    map3 UserProfile
        (field "id" int)
        (field "name" string)
        (field "email" string)
```

Um ein JSON-Objekt zu dekodieren:

```elm
import Json.Decode exposing (decodeString)

jsonString : String
jsonString = 
    """{"id": 1, "name": "John Doe", "email": "john@example.com"}"""

decoded : Result String UserProfile
decoded =
    decodeString userProfileDecoder jsonString

{- Beispiel Ausgabe:
Result.Ok { id = 1, name = "John Doe", email = "john@example.com" }
-}
```

### Elm in JSON kodieren

Um einen Elm-Wert zurück in JSON zu kodieren, nutzen Sie das Modul `Json.Encode`.

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
Verwendung:
encodeUserProfile { id = 1, name = "John Doe", email = "john@example.com" }

Beispiel Ausgabe:
"{"id":1,"name":"John Doe","email":"john@example.com"}"
-}
```

### Bibliotheken von Drittanbietern

Elm-Pakete wie `elm-json-decode-pipeline` können die Erstellung von Decodern mit einem Pipeline-Stil vereinfachen, was insbesondere beim Dekodieren komplexer Objekte praktisch ist.

Zuerst fügen Sie die Bibliothek zu Ihrem Projekt hinzu:

```shell
elm install NoRedInk/elm-json-decode-pipeline
```

Danach können Sie die Definition des Decoders wie folgt vereinfachen:

```elm
import Json.Decode exposing (int, string, succeed)
import Json.Decode.Pipeline exposing (required, decode)

userProfileDecoder : Decoder UserProfile
userProfileDecoder =
    decode UserProfile
        |> required "id" int
        |> required "name" string
        |> required "email" string

{- Verwenden Sie diesen Decoder wie zuvor mit decodeString, um JSON-Zeichenketten zu dekodieren. -}
```

Dieser Ansatz vereinfacht den Decoder, macht den Code sauberer und wartungsfreundlicher, besonders wenn die Datenstrukturen komplexer werden.
