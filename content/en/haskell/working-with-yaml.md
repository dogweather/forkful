---
title:                "Working with YAML"
date:                  2024-01-19
html_title:           "Arduino recipe: Working with YAML"
simple_title:         "Working with YAML"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/haskell/working-with-yaml.md"
---

{{< edit_this_page >}}

## What & Why?

YAML (YAML Ain't Markup Language) is a human-friendly data serialization format. Programmers use it for configuration files and data exchange because of its readability and simplicity.

## How to:

To work with YAML in Haskell, use the `yaml` package. First, install it:

```shell
cabal install yaml
```

Define a data structure, and then encode and decode YAML:

```haskell
{-# LANGUAGE OverloadedStrings #-}
import Data.YAML
import Data.ByteString (ByteString)

-- Define a data structure
data Person = Person
    { name :: String
    , age  :: Int
    } deriving (Show)

-- A sample Person instance
examplePerson :: Person
examplePerson = Person "Chris" 30

-- Serialization (Haskell to YAML)
yamlEncode :: Person -> ByteString
yamlEncode = encode

-- Deserialization (YAML to Haskell)
yamlDecode :: ByteString -> Either String Person
yamlDecode = decodeThrow

main :: IO ()
main = do
    -- Encode to YAML and print the result
    putStrLn "Encoded YAML:"
    print $ yamlEncode examplePerson
  
    -- Example YAML data
    let exampleYAML = "name: Alex\nage: 25\n"
  
    -- Decode from YAML and print the result
    putStrLn "Decoded Haskell:"
    print $ yamlDecode exampleYAML
```

Sample output for encoding and decoding:

```plaintext
Encoded YAML:
"age: 30\nname: Chris\n"
Decoded Haskell:
Right (Person {name = "Alex", age = 25})
```

## Deep Dive

YAML started in 2001, targeting data serialization and human readability. It's a popular choice for config files, like Docker Compose and GitHub Workflows. Alternatives include JSON and XML, but YAML's minimal syntax is often preferred for its clean appearance. When implementing YAML in Haskell, the key is defining data structures mapping to YAML's key-value pairs. The `yaml` package, built atop the libyaml C library, provides solid performance and compatibility.

## See Also

- Official YAML website: [https://yaml.org](https://yaml.org)
- `yaml` package on Hackage: [https://hackage.haskell.org/package/yaml](https://hackage.haskell.org/package/yaml)
- `aeson` package, for JSON in Haskell which shares similarities: [https://hackage.haskell.org/package/aeson](https://hackage.haskell.org/package/aeson)
