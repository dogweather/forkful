---
date: 2024-02-03 19:03:05.743025-07:00
description: "How to: Elixir doesn't include built-in YAML support. However, you can\
  \ use third-party libraries such as `yamerl` or `yaml_elixir` to work with YAML.\u2026"
lastmod: '2024-03-13T22:44:59.800593-06:00'
model: gpt-4-0125-preview
summary: Elixir doesn't include built-in YAML support.
title: Working with YAML
weight: 41
---

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
