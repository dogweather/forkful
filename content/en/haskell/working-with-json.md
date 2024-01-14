---
title:                "Haskell recipe: Working with json"
simple_title:         "Working with json"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/haskell/working-with-json.md"
---

{{< edit_this_page >}}

## Why
In today's technological landscape, working with data in the form of JSON (JavaScript Object Notation) has become an essential skill for any developer. JSON is a lightweight, human-readable format for exchanging data between systems and is commonly used in web and mobile applications, making it a valuable tool for building modern software.

## How To
Fortunately, with Haskell, working with JSON is a breeze. Let's start by importing the "Data.Aeson" module to access its functions and types. Then, we can use the "decode" function to parse a JSON string into a Haskell data type. For example:

```Haskell
import Data.Aeson

data Book = Book { title :: String, author :: String}

jsonString = "{\"title\": \"To Kill a Mockingbird\", \"author\": \"Harper Lee\"}"

decode jsonString :: Maybe Book
-- output: Just (Book "To Kill a Mockingbird" "Harper Lee")
```

We can also use the "encode" function to convert a Haskell data type into a JSON string. For example:

```Haskell
encode (Book "The Catcher in the Rye" "J.D. Salinger")
-- output: "{\"title\":\"The Catcher in the Rye\",\"author\":\"J.D. Salinger\"}"
```

To manipulate and access specific data in a JSON object, we can use the "Data.Map" module. This allows us to easily extract values using keys and perform operations on the data. For example:

```Haskell
import Data.Map

jsonObject = fromList [("name", "John"), ("age", "25")]

lookup "age" jsonObject
-- output: Just "25"
```

## Deep Dive
In addition to simple encoding and decoding, the "Data.Aeson" module also provides functions for working with more complex JSON structures such as arrays and nested objects. We can use the "object" function to create a JSON object by specifying a list of key-value pairs. We can also use the "array" function to create a JSON array by passing in a list of values.

Another useful function is "withObject", which allows us to modify a JSON object by providing a function that maps keys to values. This is particularly helpful for manipulating large or complex JSON data.

Additionally, the "Data.Aeson.Types" module provides tools for defining custom encoders and decoders, allowing for more control over the format and structure of our data.

## See Also
Want to learn more about working with JSON in Haskell? Here are some helpful resources to get you started:

- [Official Haskell documentation for Data.Aeson](https://hackage.haskell.org/package/aeson/docs/Data-Aeson.html)
- [A Beginner's Guide to Parsing JSON Data in Haskell](https://dysinger.net/posts/2016-08-27-json-in-haskell.html)
- [An Introduction to JSON in Haskell](https://hackernoon.com/an-introduction-to-json-in-haskell-dfb9fdb7512e)
- [Real World Haskell: JSON Parsing and Generation](http://book.realworldhaskell.org/read/json.html)