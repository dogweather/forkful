---
title:                "Elixir recipe: Working with yaml"
simple_title:         "Working with yaml"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/elixir/working-with-yaml.md"
---

{{< edit_this_page >}}

## Why
YAML is a popular data serialization format that is human-readable and easy to use. It is commonly used for configuration files, making it ideal for projects that require frequent adjustments. In the Elixir community, YAML is a preferred option for data serialization due to its simplicity and compatibility with other languages.

## How To
To work with YAML in Elixir, we will need to use a library called `YAMLixir`. This library provides functions for parsing and generating YAML documents. Let's see how we can use it to create a simple YAML document.

```
Elixir
# Load YAMLixir library
iex> {:ok, yaml} = YAMLixir.load_file("config.yaml")

# Create a YAML document
iex> document = %{author: "Jane Doe", year: 2020, languages: ["Elixir", "Python", "JavaScript"]}

# Generate YAML string from document
iex> yaml_string = YAMLixir.dump(document)
```

The output of `yaml_string` will be:

```
Elixir

--- 
author: Jane Doe
year: 2020
languages: 
- Elixir
- Python
- JavaScript
```

We can also parse a YAML string into an Elixir map using the `load/1` function. Let's see an example:

```
Elixir
# Parse YAML string
iex> yaml_map = YAMLixir.load(yaml_string)

# Access values in map
iex> yaml_map["author"]
"Jane Doe"

iex> yaml_map["languages"]
["Elixir", "Python", "JavaScript"]
```

## Deep Dive
When working with YAML in Elixir, there are several things to keep in mind. First, YAML supports a variety of data types including strings, integers, lists, and maps. However, there is no built-in support for booleans, so we need to use the `#t` and `#f` values to represent `true` and `false` respectively.

It's also important to note that YAML does not enforce data types, so there is a possibility of data loss when converting from YAML to Elixir data types. For example, YAML handles all numbers as floats, so we will need to explicitly convert them to integers if needed.

Another thing to consider is that YAML allows for comments, which can be useful for adding context to a document. However, these comments will not be included in the parsed Elixir map.

## See Also
Check out these resources to learn more about working with YAML in Elixir:

- [YAMLixir documentation](https://hexdocs.pm/yaml-elixir/YAMLixir.html)
- [Working with YAML in Elixir blog post](https://elixirschool.com/en/lessons/advanced/working-with-yaml/)
- [Elixir and YAML: untangling the tangle blog post](https://medium.com/@fishpercolator/elixir-and-yaml-untangling-the-tangle-f9670ee2ce7a)

YAML is a versatile format that can bring flexibility to your data handling in Elixir. Give it a try in your next project and see how it simplifies your configuration files!