---
title:                "Working with JSON"
date:                  2024-01-19
html_title:           "Arduino recipe: Working with JSON"
simple_title:         "Working with JSON"
programming_language: "Elm"
category:             "Elm"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/elm/working-with-json.md"
---

{{< edit_this_page >}}

## What & Why?

JSON (JavaScript Object Notation) is a text format for data exchange, akin to XML but lighter and more human-readable. Elm programmers use JSON to send and receive data to/from servers, creating dynamic, data-driven web apps.

## How to:

Elm handles JSON using the `Json.Decode` and `Json.Encode` modules. Here's a basic example:

```Elm
import Html exposing (text)
import Json.Decode exposing (string)

-- Decoding a simple JSON string
jsonString : String
jsonString = "{\"name\": \"Elm\"}"

type alias User =
    { name : String }

userNameDecoder : Json.Decode.Decoder String
userNameDecoder =
    Json.Decode.field "name" string

main =
    case Json.Decode.decodeString userNameDecoder jsonString of
        Ok name ->
            text ("Welcome, " ++ name)

        Err _ ->
            text "Whoops, something went wrong!"
```
Output: 
```
Welcome, Elm
```

## Deep Dive

JSON has been the de facto standard for web APIs since the early 2000s, displacing XML for its simplicity. While Elm is succinct and type-safe, handling JSON can be verbose due to the need for explicit decoders. 

Alternatives like Haskell use typeclasses for JSON encoding/decoding, providing more out-of-the-box functionality. However, Elm's approach helps maintain type safety and avoid runtime errors. Decoders explicitly state how to convert JSON into Elm types, and encoders do the reverse process.

## See Also

For further reading and resources:

- Elm's official JSON guide: [Working with JSON in Elm](https://guide.elm-lang.org/effects/json.html)
- Json.Decode documentation: [Elm Json.Decode](https://package.elm-lang.org/packages/elm/json/latest/Json-Decode)
- Json.Encode documentation: [Elm Json.Encode](https://package.elm-lang.org/packages/elm/json/latest/Json-Encode)
