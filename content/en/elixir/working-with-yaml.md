---
title:                "Working with yaml"
html_title:           "Elixir recipe: Working with yaml"
simple_title:         "Working with yaml"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/elixir/working-with-yaml.md"
---

{{< edit_this_page >}}

## What & Why?

Working with YAML in Elixir refers to the process of manipulating YAML files to store and retrieve data. YAML stands for "YAML Ain't Markup Language" and is a human-readable data serialization format. Programmers use YAML because it is easy to read and write, making it a popular choice for storing configuration or data files for applications.

## How to:

To work with YAML in Elixir, you will need to install the "yaml" package, which can be done by adding `{:yaml, "~> 0.9.0"}` to your project's `mix.exs` file and running `mix deps.get`. Once the package is installed, you can use it with `require YAML` in your modules.

To read a YAML file, you can use `YAML.load_file("file.yaml")`, which will return a map with the data from the file. To write to a YAML file, you can use `YAML.dump(data, "file.yaml")`, where `data` is a map or struct containing the information you want to store.

```
Elixir
# Load data from YAML file
data = YAML.load_file("config.yaml")
# Write data to YAML file
YAML.dump(data, "new_config.yaml")
```

## Deep Dive:

YAML was first released in 2001 and has gained popularity as a data serialization format due to its simplicity and readability. It is often used for configuration files in applications or for storing data in a human-readable format.

An alternative to YAML is JSON, which is also human-readable but has a stricter syntax. Elixir provides built-in support for working with JSON, but YAML can be a better choice for more complex data structures or situations where readability is more important.

Internally, the "yaml" package uses the LibYAML library, a C library for parsing and emitting YAML. This makes it faster and more efficient than other Elixir libraries that use pure Elixir implementations.

## See Also:

- [YAML tutorial](https://rollout.io/blog/yaml-tutorial-everything-you-need-get-started/)
- [Elixir "yaml" package documentation](https://hexdocs.pm/yaml/YAML.html)
- [LibYAML homepage](https://pyyaml.org/wiki/LibYAML)