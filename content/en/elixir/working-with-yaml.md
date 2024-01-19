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

YAML, short for "YAML Ain't Markup Language", is a human-readable data serialization standard that has been commonly used for configuration files, but is also used in data exchange between languages with different data structures. Developers use it since it's more readable than formats like JSON and XML, and it allows representation of complex data types, hierarchy, and references.

## How to:

Elixir doesn't have built-in YAML parsing, so we'll need an external library like `yamerl`. You can add it to your dependencies in `mix.exs`:

```Elixir
defp deps do
  [
    {:yamerl, "~> 0.8.1"}
  ]
end
```

Reading from a YAML file:

```Elixir
{:ok, yamerl_app} = Application.ensure_all_started(:yamerl)
{:ok, yaml_content} = YamerlConvenience.load_file("path/to/yourFile.yaml")

IO.inspect(yaml_content)
```

Writing to a YAML file, you’ll have to serialize Elixir data structures into YAML by manually writing a method:

```Elixir
def write_yaml(my_map) do
  yaml_string = YamerlConvenience.dump(my_map)
  File.write("path/to/yourFile.yaml", yaml_string)
end
```

Run `mix deps.get` to fetch the dependency and you're good to go!

## Deep Dive

YAML was first proposed by Clark Evans in 2001, and then developed together with Oren Ben-Kiki and Ingy döt Net. It's written to integrate well with languages like Perl, Python, JavaScript, and Ruby - but there's no native support in Elixir, hence plug-ins like `yamerl`.

If you don't need complex data types, hierarchy, or reference abilities, JSON could be easier and is built into Elixir with the `Jason` library. 

`yamerl` itself does not support YAML document creation. For that converting Elixir map into handwritten YAML string or using template libraries is required, which is something you might consider when deciding on a format for data exchange or configuration.

## See Also

For more information on YAML, visit the official YAML site: 
https://yaml.org/

To further explore `yamerl`, you can visit its repo:
https://github.com/yakaz/yamerl

For more about `Jason` as a JSON alternative:
https://hexdocs.pm/jason/readme.html