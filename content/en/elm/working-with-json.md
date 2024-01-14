---
title:                "Elm recipe: Working with json"
simple_title:         "Working with json"
programming_language: "Elm"
category:             "Elm"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/elm/working-with-json.md"
---

{{< edit_this_page >}}

## Why

JSON (JavaScript Object Notation) is one of the most commonly used data exchange formats, making it a crucial skill for any developer. Elm, being a functional and strongly-typed programming language, provides an elegant and efficient way to work with JSON data. So if you want to level up your programming skills and seamlessly handle JSON data in your applications, learning Elm is the way to go.

## How To

To start working with JSON in Elm, you need to import the `Json` module. Elm provides built-in functions to convert JSON data to and from Elm's built-in data structures. For instance, when dealing with a simple JSON object like `{ "name": "John", "age": 25 }`, you can use the `Decode.map2` function to map its properties to an `Object` type in Elm.

```Elm
import Json.Decode as Decode

userDecoder : Decoder User
userDecoder =
    Decode.map2 User
        (Decode.field "name" Decode.string)
        (Decode.field "age" Decode.int)

-- Sample output
{ name = "John", age = 25 }
```

In the above code, we are using the `Decode.map2` function to map the properties of the JSON object to the `name` and `age` fields of our `User` type in Elm. The `Decode.field` function allows us to specify the type of data we are expecting for a particular field. In this case, we are expecting a string for the `name` field and an integer for the `age` field. This way, we can easily decode and handle more complex JSON structures.

Similarly, if you want to encode Elm data types to JSON, you can use the `Json.Encode` module. It provides functions to convert Elm types like `Int`, `Float`, `String`, `Bool`, and `List` to their JSON equivalents.

```Elm
import Json.Encode as Encode

userEncoder : User -> Encode.Value
userEncoder user =
    Encode.object
        [ ( "name", Encode.string user.name )
        , ( "age", Encode.int user.age )
        ]

-- Sample output
"{ \"name\": \"John\", \"age\": 25 }"
```

In the above code, we are using the `Json.Encode.object` function to create a JSON object with the property names and values provided by our `User` type. This way, we can easily encode complex Elm data types to JSON that can be sent to a server or used in other ways.

## Deep Dive

One important thing to note when working with JSON in Elm is that the `Decode.field` function has a `Decoder` type that ensures the properties of the decoded JSON match the expected type. This helps catch errors at compile time and ensures our code is type-safe. Additionally, Elm also provides other useful functions like `Decode.maybe`, `Decode.andThen`, and `Decode.map`, to name a few, to handle more complex scenarios while decoding JSON.

## See Also

For more detailed information on working with JSON in Elm, check out the official Elm documentation and the book "Elm in Action" by Richard Feldman. You can also explore other modules like `Json.Decode.Pipeline` and `Json.Encode.Pipeline` for an alternative and more concise syntax for decoding and encoding JSON data. Happy coding! 

* [Official Elm documentation](https://guide.elm-lang.org/effects/json.html)
* [Elm in Action by Richard Feldman](https://www.manning.com/books/elm-in-action)
* [Json.Decode.Pipeline module](https://package.elm-lang.org/packages/NoRedInk/elm-decode-pipeline/latest/) 
* [Json.Encode.Pipeline module](https://package.elm-lang.org/packages/NoRedInk/elm-json-decode-pipeline/latest/)