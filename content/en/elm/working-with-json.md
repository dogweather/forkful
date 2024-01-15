---
title:                "Working with json"
html_title:           "Elm recipe: Working with json"
simple_title:         "Working with json"
programming_language: "Elm"
category:             "Elm"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/elm/working-with-json.md"
---

{{< edit_this_page >}}

## Why

If you're new to Elm, you may have heard about working with JSON. JSON, short for JavaScript Object Notation, is a popular format for storing and exchanging data on the web. In Elm, it is a common way to communicate with APIs and external services.

## How To

Working with JSON in Elm is easy and straightforward. Let's take a look at some examples to see how it's done.

To start, we need to import the `Json.Decode` module in our Elm file:
```Elm
import Json.Decode exposing (..)
```

Next, we can decode a JSON string into an Elm data structure using the `decodeString` function. This function takes in two arguments - a decoder and a JSON string, and returns a `Result` type.
```Elm
decodeString : Decoder a -> String -> Result String a
```
Let's say we have the following JSON data:
```json
{
    "name": "John Doe",
    "age": 25,
    "email": "johndoe@example.com"
}
```
We can then create a decoder for this data by using the `succeed` and `field` functions. The `succeed` function takes in a value and returns a decoder that always succeeds and returns that value. The `field` function takes in a field name and a decoder and returns a decoder that extracts the value of that field.
```Elm
type alias User = 
    { name : String
    , age : Int
    , email : String
    }

decoder : Decoder User
decoder = 
    succeed User
        |> field "name" string
        |> field "age" int
        |> field "email" string
```
Now, we can use this decoder to decode our JSON data:
```Elm
decodeString decoder "{\"name\":\"John Doe\",\"age\":25,\"email\":\"johndoe@example.com\"}"
-- This will return a Result type with a value of `Ok (User "John Doe" 25 "johndoe@example.com")`
```
If our JSON data is invalid or does not match our decoder, the `decodeString` function will return an error in the form of `Err` type.

But what if we want to encode Elm data into JSON? We can use the `encode` function from the `Json.Encode` module. This function takes in a value and returns a `Value` type, which represents any valid JSON data.
```Elm
encode : a -> Value
```
Let's say we have a list of users:
```Elm
users : List User
users = 
    [ User "John Doe" 25 "johndoe@example.com"
    , User "Jane Smith" 30 "janesmith@example.com"
    ]
```
We can encode this list into JSON using the `list` function from the `Json.Encode` module:
```Elm
encode users
-- This will return a Value type with a value of `Json.Encode.ValueList [ Json.Encode.ValueObject (fromList [("name",Json.Encode.ValueString "John Doe"),("age",Json.Encode.ValueInt 25),("email",Json.Encode.ValueString "johndoe@example.com")]), Json.Encode.ValueObject (fromList [("name",Json.Encode.ValueString "Jane Smith"),("age",Json.Encode.ValueInt 30),("email",Json.Encode.ValueString "janesmith@example.com")])]`
```

## Deep Dive

Now that we've seen some examples of working with JSON in Elm, let's dive a little deeper into some important concepts.

As you may have noticed, we used decoders and encoders in our examples. These are functions that convert between Elm data structures and JSON. Decoders help us convert JSON into Elm data, and encoders help us convert Elm data into JSON.

It's important to note that Elm has a built-in type system, so when working with JSON, we need to make sure our decoders and encoders match the type of data we want to convert. This is to ensure type safety and reduce runtime errors.

In addition, we can also use the `Json.Decode.Pipeline` module to make our decoders cleaner and more readable. This module provides helper functions to create decoders in a more functional and descriptive way.

## See Also

For more information on working with JSON in Elm, check out the official documentation:
- [JSON in Elm](https://guide.elm-lang.org/effects/json.html)
- [Json.Decode](https://package.elm-lang.org/packages/elm/json/latest/Json-Decode)
- [Json.Encode](https://package.elm-lang.org/packages/elm/json/latest/Json-Encode)
- [Json.Decode.Pipeline](https://package.elm-lang.org/packages/NoRedInk/elm-decode-pipeline/latest/)