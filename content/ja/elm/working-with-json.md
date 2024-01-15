---
title:                "「JSONとの作業」"
html_title:           "Elm: 「JSONとの作業」"
simple_title:         "「JSONとの作業」"
programming_language: "Elm"
category:             "Elm"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/elm/working-with-json.md"
---

{{< edit_this_page >}}

## Why

JSON is commonly used for data exchange between systems and is a popular format for APIs, making it an important skill for developers. With Elm's strong type system and intuitive syntax, working with JSON becomes even easier.

## How To

Working with JSON in Elm is made simple with the Json.Decode and Json.Encode modules. First, we need to import them into our project:

```Elm
import Json.Decode exposing (..)
import Json.Encode exposing (..)
```

To decode JSON into Elm values, we use the `decodeValue` function and specify a decoder with the `map` function. For example, if we have a JSON object with a `name` field, we can decode it into an Elm `person` record like this:

```Elm
type alias Person =
    { name : String }

personDecoder : Decoder Person
personDecoder =
    map Person
        (field "name" string)

jsonString =
    """
    { "name": "John Doe" }
    """

decodedPerson =
    jsonString
        |> decodeValue personDecoder
        |> Result.withDefault { name = "" }
-- decodedPerson = { name = "John Doe" }
```

To encode Elm values into JSON, we use the `encode` function. For example, if we have the same `person` record and want to encode it back into JSON:

```Elm
encodedPerson =
    person
        |> encode
-- encodedPerson = { "name": "John Doe" }
```

## Deep Dive

Elm's `Decoder` and `Encoder` types provide a powerful abstraction for working with JSON. The `map` function allows us to transform and manipulate the data, while the built-in decoder functions such as `field`, `string`, and `int` make it easy to handle different data types. Additionally, Elm's type system ensures that our code is error-free and prevents unexpected JSON values from being decoded.

## See Also

- ["Encoding and Decoding JSON with Elm"](https://guide.elm-lang.org/effects/json.html) from the official Elm guide
- ["Working with JSON in Elm"](https://blog.jenkster.com/2016/01/working-with-json-in-elm.html) by Jenkster blog
- ["Decode Pipeline"](https://package.elm-lang.org/packages/elm/json/latest/Json-Decode#)! for a comprehensive list of decoder functions.