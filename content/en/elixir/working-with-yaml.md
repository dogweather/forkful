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

## Why

If you're a programmer looking to work with data in a more human-readable format, YAML might just be the perfect solution for you. Its simple syntax and flexibility make it a popular choice for data serialization and configuration management.

## How To

To start using YAML in your Elixir projects, you'll need to add the [yaml](https://github.com/KronicDeth/yaml_elixir) package as a dependency in your `mix.exs` file:

```elixir
defp deps do
  [{:yaml, "~> 0.0.0"}]
end
```

Next, you'll need to add `:yaml` to your `applications` list in the same `mix.exs` file in order to make it available in your application:

```elixir
def application do
  [applications: [:yaml]]
end
```

Now, let's take a look at how we can write and read YAML data in Elixir using the `YAML` module from the `yaml` package:

```elixir
# Writing YAML data
YAML.write("foo.yml", %{name: "John", age: 28})
# Output: "---\n:name: John\n:age: 28\n"

# Reading YAML data
YAML.read("foo.yml")
# Output: %{name: "John", age: 28}
```

## Deep Dive

YAML, which stands for "YAML Ain’t Markup Language", is a human-readable data serialization language that is easily writable by humans and conveniently parsed by machines. It supports a wide range of data types including strings, integers, booleans, arrays, and maps, making it a versatile choice for storing and transferring data. In addition to Elixir, YAML is also supported by other popular programming languages such as Python, Ruby, and JavaScript.

One of the key features of YAML is its ability to use indentation for structure, making it easy to read and understand the data. It also allows for comments, which can be helpful for adding notes or explanations for different parts of the data. Another advantage of YAML is that its syntax is simple and consistent, making it less prone to errors and easier to maintain compared to other data formats like JSON.

In Elixir, the `YAML` module from the `yaml` package provides us with useful functions for reading and writing YAML data. It also supports more advanced features like custom type handling and tag mappings. You can explore these features and more by checking out the official documentation for the `yaml` package.

## See Also

- [Official `yaml` Package Documentation](https://hexdocs.pm/yaml/readme.html)
- [YAML Specification](https://yaml.org/spec/)