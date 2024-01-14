---
title:                "Elm recipe: Working with yaml"
simple_title:         "Working with yaml"
programming_language: "Elm"
category:             "Elm"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/elm/working-with-yaml.md"
---

{{< edit_this_page >}}

##Why

If you're looking for a reliable and easy-to-use data serialization language, you might want to consider YAML. It's a popular choice among developers for its human-readable format, making it easy to understand and work with.

##How To

To get started with YAML in Elm, you'll first need to install the necessary package. Open your terminal and run the following command:

```Elm 
elm install NoRedInk/elm-json-decode-pipeline
```

Next, you can create a new file and import the needed modules:

```Elm 
import Json.Decode as Decode exposing (..)
import Json.Decode.Pipeline exposing (..)
```

Now, let's say we have a YAML file called "users.yaml" with the following data:

```yaml
- name: John
  age: 25
- name: Emily
  age: 30
```

We can use the "decodeValue" function from the Json.Decode.Pipeline module to decode the YAML data into a list of users:

```Elm
type alias User =
    { name : String
    , age : Int
    }

decodeUsers : Decode.Decoder (List User)
decodeUsers =
    Decode.list
        (Decode.map2 User
            (Decode.field "name" Decode.string)
            (Decode.field "age" Decode.int)
        )

users : List User
users =
    Decode.decodeValue decodeUsers
        (Decode.yamlString "users.yaml")
```

The resulting output will be:

```Elm
[ { name = "John", age = 25 }, { name = "Emily", age = 30 } ]
```

##Deep Dive

While the above example shows a basic usage of YAML in Elm, there are many other features and capabilities that make it a powerful tool for data serialization. Some notable features include:

- Support for comments and block scalars, making it easy to add context and special formatting to your data.
- Ability to include complex data structures such as maps and sequences within the same YAML file.
- Support for anchors and aliases, allowing you to easily reuse and reference data within your YAML file.

It's also worth mentioning that YAML is often used in conjunction with JSON, as it offers more flexibility and readability compared to JSON. However, both formats are useful for different purposes and can coexist within the same project.

##See Also

If you want to learn more about YAML in Elm, here are some helpful resources:

- Official YAML website: https://yaml.org/
- Elm package for working with YAML: https://package.elm-lang.org/packages/NoRedInk/elm-json-decode-pipeline/latest/
- Alternative YAML parser for Elm: https://package.elm-lang.org/packages/jreut/elm-yaml/latest/

Happy coding!