---
title:                "Working with YAML"
html_title:           "Arduino recipe: Working with YAML"
simple_title:         "Working with YAML"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/elixir/working-with-yaml.md"
---

{{< edit_this_page >}}

## What & Why?

Working with YAML means parsing and generating YAML-formatted data, a human-readable data serialization standard. Programmers do it for configuration files, data exchange, and because it's more readable than JSON or XML for complex data structures.

## How to:

Elixir doesn't include built-in YAML support, but you can use the `yamerl` library. First, add `yamerl` to your `mix.exs` file:

```elixir
defp deps do
  [{:yamerl, "~> 0.8"}]
end
```

After running `mix deps.get`, you can parse YAML:

```elixir
yml_data = """
name: John Doe
age: 30
langs:
  - Elixir
  - Ruby
  - Haskell
"""

parsed_data = :yamerl_constr.string(yml_data) |> Enum.take(1)
IO.inspect(parsed_data)
```

This will output:

```elixir
[
  %{
    "age" => 30,
    "langs" => ["Elixir", "Ruby", "Haskell"],
    "name" => "John Doe"
  }
]
```

And to convert Elixir data to YAML:

```elixir
data = %{
  name: "John Doe",
  age: 30,
  langs: ["Elixir", "Ruby", "Haskell"]
}

yml_string = :yamerl.encode(data)
IO.puts yml_string
```

This prints:

```yaml
---
age: 30
langs:
  - Elixir
  - Ruby
  - Haskell
name: John Doe
```

## Deep Dive

YAML, standing for "YAML Ain't Markup Language" (a recursive acronym), has been around since 2001. JSON and XML can serve similar purposes but YAML's focus on readability makes it popular for configurations. `yamerl`, an Erlang lib adapted for Elixir via interoperability, is a solid choice for Elixir devs. Remember, YAML is indentation-sensitive, making parsing a little trickier compared to JSON.

## See Also

- Official `yamerl` GitHub repo: https://github.com/yakaz/yamerl
- Elixir `hexdocs` for YAML libs: https://hex.pm/packages?search=yaml&sort=recent_downloads
- YAML official site for specs and more: https://yaml.org
- Elixir School for Elixir learning: https://elixirschool.com/en/
