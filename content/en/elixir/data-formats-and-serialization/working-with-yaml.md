---
title:                "Working with YAML"
aliases: - /en/elixir/working-with-yaml.md
date:                  2024-02-03T19:03:05.743025-07:00
model:                 gpt-4-0125-preview
simple_title:         "Working with YAML"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/elixir/working-with-yaml.md"
---

{{< edit_this_page >}}

## What & Why?

YAML, short for YAML Ain't Markup Language, is a human-readable data serialization standard commonly used for configuration files and data exchange between languages with different data structures. Programmers use it because of its simplicity and its ability to easily represent complex hierarchical data.

## How to:

Elixir doesn't include built-in YAML support. However, you can use third-party libraries such as `yamerl` or `yaml_elixir` to work with YAML. Here, we'll focus on `yaml_elixir` for its ease of use and comprehensive features.

First, add `yaml_elixir` to your mix.exs dependencies:

```elixir
defp deps do
  [
    {:yaml_elixir, "~> 2.9"}
  ]
end
```

Then, run `mix deps.get` to fetch the new dependency.

### Reading YAML

Given a simple YAML file, `config.yaml`, which looks like this:

```yaml
database:
  adapter: postgres
  username: user
  password: pass
```

You can read this YAML file and convert it to an Elixir map like so:

```elixir
defmodule Config do
  def read do
    {:ok, content} = YamlElixir.read_from_file("config.yaml")
    content
  end
end

# Sample usage
Config.read()
# Output: 
# %{
#   "database" => %{
#     "adapter" => "postgres",
#     "username" => "user",
#     "password" => "pass"
#   }
# }
```

### Writing YAML

To write a map back to a YAML file:

```elixir
defmodule ConfigWriter do
  def write do
    content = %{
      database: %{
        adapter: "mysql",
        username: "root",
        password: "s3cret"
      }
    }
    
    YamlElixir.write_to_file("new_config.yaml", content)
  end
end

# Sample usage
ConfigWriter.write()
# This will create or overwrite `new_config.yaml` with the specified content
```

Notice how `yaml_elixir` allows for a straightforward translation between YAML files and Elixir data structures, making it an excellent choice for Elixir programmers needing to work with YAML data.
