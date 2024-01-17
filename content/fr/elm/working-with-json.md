---
title:                "Travailler avec json"
html_title:           "Elm: Travailler avec json"
simple_title:         "Travailler avec json"
programming_language: "Elm"
category:             "Elm"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/elm/working-with-json.md"
---

{{< edit_this_page >}}

## What & Why?

JSON (Javascript Object Notation) is a popular data format used by programmers to store and transmit data. It is easy to read and write for both humans and machines, making it a preferred choice for data exchange.

## How to:

In Elm, working with JSON is made simple through the elm/json package. First, we need to import the package by adding `import Json` to the top of our code. Then, we can use the `json` function to convert our data into a JSON value.

```Elm
import Json

data = { name = "John", age = 25, city = "Paris" }
jsonData = Json.encode 2 data

-- Output:
-- { "age": 25, "city": "Paris", "name": "John" }
```

To decode a JSON value, we can use the `Decoder` module from the elm/json package. We need to specify the structure of our data using the `decode` function, and then use `Json.Decode.decodeString` to convert the JSON into our desired type.

```Elm
import Json.Decode as Decode

type alias User = 
  { name : String
  , age : Int
  , city : String
  }

userDecoder : Decode.Decoder User
userDecoder = 
  Decode.map3 User
    (Decode.field "name" Decode.string)
    (Decode.field "age" Decode.int)
    (Decode.field "city" Decode.string)

jsonString = "{ name: \"John\", age: 25, city: \"Paris\" }"
result = Decode.decodeString userDecoder jsonString

-- Output:
-- Ok (User { name = "John", age = 25, city = "Paris" })
```

## Deep Dive:

JSON was first introduced in 2001 and has since become a standard data format for web development. Its simplicity and lightweight design make it a popular choice compared to other formats like XML. Elm’s type system and immutability make it a great fit for working with JSON, ensuring clean and reliable code.

An alternative to using the `json` function in Elm is to create a custom encoder and decoder for our data type using the `Json.Encode` and `Json.Decode` modules. This gives us more control over how our data is encoded and decoded.

## See Also:

To learn more about working with JSON in Elm, you can check out the official documentation for the `elm/json` package or the Elm Guide. You can also explore other packages like `elm/bytes` and `NoRedInk/elm-json-decode-pipeline` for more advanced JSON manipulation techniques.