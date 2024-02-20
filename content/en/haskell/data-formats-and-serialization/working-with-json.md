---
date: 2024-02-03 19:03:17.276514-07:00
description: "Working with JSON (JavaScript Object Notation) in Haskell involves parsing\
  \ JSON data into Haskell types and converting Haskell types back into JSON.\u2026"
lastmod: 2024-02-19 22:05:18.611128
model: gpt-4-0125-preview
summary: "Working with JSON (JavaScript Object Notation) in Haskell involves parsing\
  \ JSON data into Haskell types and converting Haskell types back into JSON.\u2026"
title: Working with JSON
---

{{< edit_this_page >}}

## What & Why?
Working with JSON (JavaScript Object Notation) in Haskell involves parsing JSON data into Haskell types and converting Haskell types back into JSON. Programmers do this to enable their Haskell applications to exchange data with web services or APIs seamlessly, a common practice in modern software development for cross-platform data interchange.

## How to:
Haskell doesn't have built-in support for JSON like JavaScript, but with the help of third-party libraries such as **Aeson**, handling JSON becomes straightforward. Aeson provides both high-level and low-level functions for encoding (converting Haskell values to JSON) and decoding (parsing JSON into Haskell values).

### Installing Aeson
First, add Aeson to your project's dependencies by updating your `.cabal` file or using Stack or Cabal directly:

```shell
cabal update && cabal install aeson
```
or, if you are using Stack:
```shell
stack install aeson
```

### Parsing JSON
Let's start with a basic example of decoding JSON data into a Haskell type. Suppose we have the following JSON representing a person:

```json
{
  "name": "John Doe",
  "age": 30
}
```

First, define a corresponding Haskell data type and make it an instance of `FromJSON`:

```haskell
{-# LANGUAGE DeriveGeneric #-}

import GHC.Generics (Generic)
import Data.Aeson (FromJSON, decode)
import qualified Data.ByteString.Lazy as B

data Person = Person
  { name :: String
  , age :: Int
  } deriving (Generic, Show)

instance FromJSON Person

-- Function to decode JSON from a file
decodePerson :: FilePath -> IO (Maybe Person)
decodePerson filePath = do
  personJson <- B.readFile filePath
  return $ decode personJson
```
Usage:
Assuming `person.json` contains the JSON data shown above, run:
```haskell
main :: IO ()
main = do
  maybePerson <- decodePerson "person.json"
  print maybePerson
```
Sample Output:
```haskell
Just (Person {name = "John Doe", age = 30})
```

### Encoding Haskell Values as JSON
To convert a Haskell value back to JSON, you need to make your type an instance of `ToJSON` and then use `encode`.

```haskell
import Data.Aeson (ToJSON, encode)
import GHC.Generics (Generic)

-- Assuming the Person type from before

instance ToJSON Person

encodePerson :: Person -> B.ByteString
encodePerson = encode

main :: IO ()
main = do
  let person = Person "Jane Doe" 32
  putStrLn $ show $ encodePerson person
```
Sample Output:
```json
{"name":"Jane Doe","age":32}
```

These examples demonstrate the basics of working with JSON in Haskell using Aeson. Remember, Aeson offers much more, including custom parsing rules, working with complex nested JSON, and much more, suitable for various needs and scenarios.
