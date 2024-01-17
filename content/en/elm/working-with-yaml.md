---
title:                "Working with yaml"
html_title:           "Elm recipe: Working with yaml"
simple_title:         "Working with yaml"
programming_language: "Elm"
category:             "Elm"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/elm/working-with-yaml.md"
---

{{< edit_this_page >}}

## What & Why?

Working with YAML in Elm is a way to store and organize data in a human-readable and easily configurable format. Programmers use YAML to create and modify configurations for projects or applications, as well as storing data for web applications. It allows for a concise and organized way of representing complex data structures, making it a popular choice for managing settings and data in code.

## How to:

Using YAML in Elm is quite straightforward. The ```Yaml.Decode``` and ```Yaml.Encode``` packages, available on Elm's package repository, provide functions to parse YAML data into Elm data types and vice versa.

To begin, start by importing the needed packages:

```
import Yaml.Decode exposing (..)
import Yaml.Encode as Encode
```

Next, let's take a look at how to decode YAML into Elm data types. The ```Yaml.Decode``` package provides a ```decodeString``` function that takes in a YAML string and returns a decoder. We can then use the ```decode``` function to convert this decoder to a specific data type, such as a ```Dict``` or a ```List```.

```
yamlString =
"""
name: John Doe
age: 25
"""

decoder = decodeString yamlString
    |> andThen dict
```

This will return a ```Dict``` in the form of ```Dict String String```. To decode the YAML into a different data type, simply change the function used in ```andThen``` to the desired data type. Similarly, to encode Elm data types into YAML, the ```Encode.encode``` function can be used.

```
elmData =
    dict
        [ ( "name", "John Doe" )
        , ( "age", "25" )
        ]

encodedYaml = encode elmData
```

This will return a YAML string in the form of ```"name: John Doe\nage: 25"```.

## Deep Dive:

YAML, which stands for "YAML Ain't Markup Language", was first created in 2001 by Clark Evans and Ingy döt Net. It was designed to be a human-readable data serialization language and is often used in place of JSON due to its more flexible syntax. It supports comments, multiple data types, and inheritance, making it a more robust choice for complex data structures.

Alternatives to using YAML in Elm include JSON and XML. However, compared to these options, YAML offers a more compact and readable syntax, making it a popular choice for managing settings and data in code.

The implementation of YAML in Elm is based on the YAML specification, version 1.2. The ```Yaml.Decode``` package uses a parser written in Elm to convert YAML syntax into Elm data types, while the ```Yaml.Encode``` package uses a custom encoder that translates Elm data types into YAML syntax.

## See Also:

- Official YAML website: https://yaml.org/
- Elm package repository for ```Yaml.Decode``` and ```Yaml.Encode```: https://package.elm-lang.org/packages/elm/core/latest/
- YAML tutorial for beginners: https://docs.ansible.com/ansible/latest/reference_appendices/YAMLSyntax.html