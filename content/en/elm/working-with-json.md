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

## What & Why?

Working with JSON is a common task in programming, especially in web development. JSON (JavaScript Object Notation) is a lightweight data interchange format that is easy for humans to read and write, and for machines to parse and generate. It is used to transmit data between a server and a web application, making it an essential tool for modern web development.

Programmers use JSON to structure and organize data so that it can be easily transported and manipulated. It is highly versatile and is a standard format for exchanging data between different systems, making it a key component in the development process.

## How to:

Coding in Elm to work with JSON is straightforward and follows a few simple steps. First, we need to import the `Json.Decode` module, which contains functions to help us parse and work with JSON data. Next, we use the `Json.Decode.decodeValue` function to decode the JSON data into a specific type, such as a record or list. Finally, we can use the decoded data in our program as needed.

```
import Json.Decode exposing (..)

type alias User = 
  { name: String
  , age: Int
  }

jsonUser = 
  """
  {"name": "John Doe", "age": 25}
  """

decodedData = decodeValue jsonUser (object2 User "name" string "age" int)
```

In this example, the `User` type specifies the structure of our data, and the `object2` function takes in the properties of the JSON object and maps them to the corresponding types. The `decodedData` will now contain the decoded JSON data in the form of a `User` record, which we can use in our program.

## Deep Dive

JSON has become the standard format for exchanging data due to its simplicity and readability. It is derived from JavaScript and has a similar syntax, making it easy for developers to work with. It was first introduced by Douglas Crockford in 2001 and has since become widely adopted across different programming languages.

Alternative formats, such as XML, were widely used before JSON's emergence, but due to its verbosity and complexity, it has largely been replaced by JSON in web development.

When working with JSON in Elm, it's essential to understand how the `Json.Decode` module works and how to utilize it to convert the data into a usable format. Functions such as `map`, `objectN`, and `list` can be used to specify the expected structure of the JSON data and decode it accordingly.

## See Also

For more information on working with JSON in Elm, check out the official Elm guide on JSON (https://guide.elm-lang.org/json/). You can also refer to the Elm package website for a list of available functions and resources (https://package.elm-lang.org/packages/elm/json/latest/).