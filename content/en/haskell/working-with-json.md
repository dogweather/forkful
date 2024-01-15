---
title:                "Working with json"
html_title:           "Haskell recipe: Working with json"
simple_title:         "Working with json"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/haskell/working-with-json.md"
---

{{< edit_this_page >}}

## Why

JSON (JavaScript Object Notation) is a popular data interchange format used in many web applications. Working with JSON in Haskell allows developers to easily parse, manipulate, and generate JSON data, making it a valuable skill for those working in the web development industry.

## How To

To start working with JSON in Haskell, we will need to import the `aeson` library which provides functions for encoding and decoding JSON data. We can do this by adding the following line at the beginning of our program:

```Haskell
import Data.Aeson
```

### Encoding

To encode a Haskell data type into JSON, we can use the `toJSON` function from the `Data.Aeson` module. Let's say we have a data type called `Person`:

```Haskell
data Person = Person
  { name :: String
  , age :: Int
  , occupation :: String
  }
```

We can define an instance of `ToJSON` for `Person` like this:

```Haskell
instance ToJSON Person where
  toJSON (Person name age occupation) =
    object [ "name" .= name
           , "age" .= age
           , "occupation" .= occupation
           ]
```

Then, we can encode a `Person` into JSON using `toJSON`:

```Haskell
encode (Person "John" 32 "Software Developer")
-- Output: {"name":"John","age":32,"occupation":"Software Developer"}
```

### Decoding

To decode JSON data into a Haskell data type, we can use the `decode` function from the `Data.Aeson` module. For example, if we have the following JSON data:

```Haskell
jsonData = "{ \"name\": \"Jane\", \"age\": 28, \"occupation\": \"Data Analyst\" }"
```
 
and a corresponding instances of `FromJSON` for `Person`:

```Haskell
instance FromJSON Person where
  parseJSON (Object v) =
    Person <$> v .: "name"
           <*> v .: "age"
           <*> v .: "occupation"
```

we can decode it into a `Person` using `decode`:

```Haskell
decode jsonData :: Maybe Person
-- Output: Just (Person {name = "Jane", age = 28, occupation = "Data Analyst"})
```

### Deep Dive

Working with JSON in Haskell also allows us to handle more complex data structures. For instance, we can use the `Data.Aeson.Types` module to define custom data types for specific JSON data formats.

Additionally, the `aeson` library provides functions for handling common JSON data types, such as arrays and nested objects. These functions can be found in the `Data.Aeson.Types.Array` and `Data.Aeson.Lens` modules, respectively.

## See Also

- [Hackage: aeson](https://hackage.haskell.org/package/aeson)
- [Official JSON documentation](https://www.json.org/json-en.html)
- [Aeson tutorial](http://kazu-yamamoto.hatenablog.jp/entry/2020/07/27/073101)