---
title:                "Working with JSON"
html_title:           "Arduino recipe: Working with JSON"
simple_title:         "Working with JSON"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/haskell/working-with-json.md"
---

{{< edit_this_page >}}

## What & Why?

JSON (JavaScript Object Notation) is a text-based data format for storing and transporting data. Programmers use it because it's lightweight, easy to read/write, and language-independent.

## How to:

In Haskell, we handle JSON using the `aeson` library. To get started, import it and define a type that corresponds to your expected JSON structure.

```Haskell
{-# LANGUAGE DeriveGeneric #-}

import Data.Aeson
import GHC.Generics

-- Assuming we have a JSON object with a "name" and an "age"

data Person = Person 
  { name :: String
  , age  :: Int
  } deriving (Generic, Show)

instance FromJSON Person
instance ToJSON Person

-- Parsing JSON string
main :: IO ()
main = do
  let jsonString = "{\"name\":\"John\", \"age\":30}"
  let maybePerson = decode jsonString :: Maybe Person
  case maybePerson of
    Nothing -> putStrLn "Error parsing JSON."
    Just person -> print person
```

Output:
```
Person {name = "John", age = 30}
```

## Deep Dive

- **History**: JSON's design was influenced by a subset of JavaScript syntax, and it first gained traction as a simple alternative to XML.
- **Alternatives**: While JSON is king for web APIs, alternatives like XML, YAML, or even Protocol Buffers might be chosen based on context and requirements.
- **Implementation Details**: `aeson` uses Haskell’s type system to match JSON structures to Haskell types. Parsing is done via typeclasses like `FromJSON`, and encoding through `ToJSON`.

## See Also

- `aeson` package documentation: [https://hackage.haskell.org/package/aeson](https://hackage.haskell.org/package/aeson)
- Real-world JSON APIs to practice with: [https://jsonplaceholder.typicode.com/](https://jsonplaceholder.typicode.com/)
- JSON specification: [https://www.json.org/json-en.html](https://www.json.org/json-en.html)
