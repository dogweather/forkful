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

## Why

Do you work with configuration files or data serialization? YAML is a simple and widely-used format for storing and exchanging data, making it a valuable tool for developers and data analysts.

## How To

Firstly, make sure you have the `yaml` library installed in your Haskell environment. Next, import the necessary modules:

```
import Data.Yaml
import qualified Data.Text as T
```

To parse a YAML file, use the `decodeFile` function and specify the file path:

```
users <- decodeFile "users.yaml" :: IO (Maybe [User])
```

Note that specifying the data type (here, `Maybe [User]`) is necessary for the compiler to infer the correct type. 

To convert a Haskell data type into YAML, use the `encode` function:

```
let user = User "John Smith" "jsmith@mail.com" 25
yaml <- T.putStrLn $ encode user
```

Here, we are using the `putStrLn` function from the `Data.Text` module to display the resulting YAML output.

You can also use YAML directly in your code by using the `decode` function to convert a `ByteString` into a YAML `Value` and then use the `parseMaybe` function to parse the `Value` into a specific data type:

```
import Data.ByteString (readFile)
import Data.Yaml
import Data.Maybe (fromJust)

yamlString <- readFile "user.yaml"
let user = decode (T.unpack yamlString) :: Maybe Value
let parsedUser = fromJust $ parseMaybe parseJSON user :: User
```

## Deep Dive

One of the key features of YAML is its human-readable and intuitive syntax. It uses indentation to define data structures, eliminating the need for brackets or parentheses. For example, a list of animals can be represented as:

```
- Lion
- Tiger
- Bear
```

YAML also supports complex data types, including maps and sequences. To represent a map, use `key: value` pairs, while sequences use `-` before each element. For more information on YAML syntax and features, check out the official documentation at https://yaml.org/.

YAML also allows for comments, denoted by a `#` symbol. Comments can be used to provide extra information or to temporarily disable a specific entry.

## See Also

- Official YAML documentation: https://yaml.org/
- `yaml` library documentation: https://hackage.haskell.org/package/yaml