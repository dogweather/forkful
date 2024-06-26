---
date: 2024-02-03 19:03:19.813824-07:00
description: "How to: Elm treats JSON handling with explicitness and safety, primarily\
  \ using the `Json.Decode` and `Json.Encode` modules. To start working with JSON,\u2026"
lastmod: '2024-03-13T22:45:00.029610-06:00'
model: gpt-4-0125-preview
summary: Elm treats JSON handling with explicitness and safety, primarily using the
  `Json.Decode` and `Json.Encode` modules.
title: Working with JSON
weight: 38
---

## How to:
Elm treats JSON handling with explicitness and safety, primarily using the `Json.Decode` and `Json.Encode` modules. To start working with JSON, you first need to define a decoder for your data type. Let's assume we're dealing with a simple user profile object.

Firstly, define your Elm type:

```elm
type alias UserProfile = 
    { id : Int
    , name : String
    , email : String
    }
```

### Decoding JSON into Elm
To decode a JSON string into the `UserProfile` type, create a decoder:

```elm
import Json.Decode exposing (Decoder, int, string, field, map3)

userProfileDecoder : Decoder UserProfile
userProfileDecoder =
    map3 UserProfile
        (field "id" int)
        (field "name" string)
        (field "email" string)
```

To decode a JSON object:

```elm
import Json.Decode exposing (decodeString)

jsonString : String
jsonString = 
    """{"id": 1, "name": "John Doe", "email": "john@example.com"}"""

decoded : Result String UserProfile
decoded =
    decodeString userProfileDecoder jsonString

{- Sample Output:
Result.Ok { id = 1, name = "John Doe", email = "john@example.com" }
-}
```

### Encoding Elm into JSON
To encode an Elm value back into JSON, leverage the `Json.Encode` module.

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
Usage:
encodeUserProfile { id = 1, name = "John Doe", email = "john@example.com" }

Sample Output:
"{"id":1,"name":"John Doe","email":"john@example.com"}"
-}
```

### Third-Party Libraries
Elm packages like `elm-json-decode-pipeline` can simplify the creation of decoders using a pipeline style, which is especially handy for decoding complex objects.

First, add the library to your project:

```shell
elm install NoRedInk/elm-json-decode-pipeline
```

Then, you can simplify the decoder definition as follows:

```elm
import Json.Decode exposing (int, string, succeed)
import Json.Decode.Pipeline exposing (required, decode)

userProfileDecoder : Decoder UserProfile
userProfileDecoder =
    decode UserProfile
        |> required "id" int
        |> required "name" string
        |> required "email" string

{- Use this decoder as before with decodeString to decode JSON strings. -}
```

This approach simplifies the decoder, making the code cleaner and more maintainable, especially as the data structures become more complex.
