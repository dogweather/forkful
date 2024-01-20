---
title:                "Working with yaml"
html_title:           "Arduino recipe: Working with yaml"
simple_title:         "Working with yaml"
programming_language: "Elm"
category:             "Elm"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/elm/working-with-yaml.md"
---

{{< edit_this_page >}}

## What & Why?

YAML, a human-friendly data serialization standard, is used for configuration files and data exchange. Programmers dig it because it's clear, easy to read, and widely adopted across tools and languages.

## How to:

Elm doesn't have built-in YAML parsing, so you typically convert YAML to JSON using an external tool and then work with it in Elm using the `elm/json` library.

```elm
import Json.Decode exposing (Decoder, field, string, int, decodeValue)

type alias User =
    { name : String
    , age : Int
    }

userDecoder : Decoder User
userDecoder =
    Json.Decode.map2 User
        (field "name" string)
        (field "age" int)

jsonString : String
jsonString =
    """
    {
        "name": "Jane Doe",
        "age": 25
    }
    """

parseResult : Result String User
parseResult =
    jsonString
        |> Json.Decode.decodeString userDecoder

-- Sample output: Result.Ok { name = "Jane Doe", age = 25 }
```
Elm code processes JSON, the equivalent of your YAML after conversion.

## Deep Dive:

YAML's simplicities trace back to the early 2000s as a human-legible alternative to XML. While Elm doesn't parse YAML natively, JSON is a breeze, thanks to `elm/json`. Some folks use third-party services or tools like `yaml-to-json.com` or even write a bit of server-side code in Node.js or Python to do the YAML-to-JSON dance. Remember, Elm shines with JSON, so this two-step conversion is the workaround the Elm community generally uses.

## See Also:

- Elm JSON package: https://package.elm-lang.org/packages/elm/json/latest/
- Online YAML to JSON converter: https://yaml-to-json.com/
- JSON-to-Elm type generator: https://noredink.github.io/json-to-elm/