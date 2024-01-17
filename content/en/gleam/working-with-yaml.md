---
title:                "Working with yaml"
html_title:           "Gleam recipe: Working with yaml"
simple_title:         "Working with yaml"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/gleam/working-with-yaml.md"
---

{{< edit_this_page >}}

## What & Why?

Working with YAML is a common task for programmers, as it allows them to easily store and organize configuration data in a human-readable format. YAML stands for "YAML Ain't Markup Language" and is often used as an alternative to XML or JSON for configuration files and data serialization.

## How to:

To work with YAML in Gleam, the first step is to import the `gleam/yaml` module:

```
import gleam/yaml
```

Then, to read a YAML file and convert it to a Gleam type, you can use the `from_string` function:

```
Gleam.from_string(yaml_string) // Converts the YAML string to a Gleam type
```

To convert a Gleam type back to YAML, you can use the `to_string` function:

```
Yaml.to_string(gleam_type) // Converts the Gleam type to a YAML string
```

Here's a simple example of using YAML in Gleam:

```
let example_yaml = "
name: Gleam
year: 2019
is_awesome: true
"
let config = Yaml.from_string(example_yaml)
```

The `config` variable will then contain a Gleam map with the corresponding keys and values from the YAML string. This makes it easy to access the data programmatically.

## Deep Dive

YAML was first introduced in 2001 and has since become a popular choice for configuration files due to its simplicity and readability. It is widely supported by many programming languages and has libraries available for easy integration.

As an alternative to XML or JSON, YAML offers a more human-friendly syntax, making it easier to read and understand even for non-technical people. It also allows for multi-line strings, making it a great choice for writing documentation or other text-heavy data.

In Gleam, the `gleam/yaml` module uses the `libyaml` library, a lightweight YAML C library, for its parsing and serialization. This ensures efficient and reliable handling of YAML files.

## See Also

- [YAML Specification](https://yaml.org/spec/) - Official YAML specification
- [Gleam Documentation](https://gleam.run/documentation/) - Official Gleam documentation 
- [libyaml](https://pyyaml.org/wiki/LibYAML) - Lightweight YAML C library used by Gleam's `gleam/yaml` module