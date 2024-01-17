---
title:                "Working with yaml"
html_title:           "Haskell recipe: Working with yaml"
simple_title:         "Working with yaml"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/haskell/working-with-yaml.md"
---

{{< edit_this_page >}}

## What & Why?

Working with YAML is a common practice among programmers as it allows for easy data serialization and configuration management. YAML is a human-readable data format that is used to represent complex data structures in a simple and concise manner. It is especially useful for configuring applications, storing data, and transferring data between systems.

## How to:

To work with YAML in Haskell, you can use the popular library "yaml" which provides functions for reading, writing, and parsing YAML data. First, make sure to have the "yaml" library installed by running the command `cabal install yaml` in your terminal.

Next, import the library in your Haskell file using the following statement: `import Data.Yaml`.

### Parsing YAML

To parse a YAML file, use the `decodeFile` function which takes a file path as its argument and returns a `Data.Maybe` type which is either `Just` the parsed YAML data or `Nothing` if there was an error.

```Haskell
import Data.Yaml

main = do
    result <- decodeFile "my-data.yaml"
    case result of
        Just data -> print data
        Nothing -> putStrLn "Error parsing YAML"
```

### Writing YAML

To write YAML data to a file, use the `encodeFile` function which takes a file path and the data to be written as arguments.

```Haskell
import Data.Yaml

main = do
    let data = ["Haskell", "YAML", "Tutorial"]
    encodeFile "my-data.yaml" data
```

### Working with YAML data

YAML data is represented in Haskell as a `Value` type, which is a data structure that can be accessed using normal Haskell pattern matching and functions. For example, to access a key-value pair in a YAML mapping, you can use the `lookup` function.

```Haskell
import Data.Yaml

main = do
    result <- decodeFile "my-data.yaml"
    case result of
        Just (Object mapping) -> print $ lookup "key" mapping
        Nothing -> putStrLn "Error parsing YAML"
```

## Deep Dive

YAML was first introduced in 2001 as a more human-readable alternative to other serialization formats such as XML and JSON. It is based on the YAML Ain't Markup Language (YAML) specification, which is a data serialization language designed for easy configuration and data exchange.

As an alternative to YAML, some programmers use JSON or TOML. TOML is a newer configuration file format that is aimed at making configuration files more readable and easier to edit.

The "yaml" library in Haskell is implemented using the "libyaml" C library, which provides high-performance and efficient parsing, emitting, and parsing of YAML data.

## See Also

- [YAML.org](https://yaml.org/)
- [Hackage: yaml library](https://hackage.haskell.org/package/yaml)
- [TOML](https://toml.io/)
- [JSON](https://www.json.org/)