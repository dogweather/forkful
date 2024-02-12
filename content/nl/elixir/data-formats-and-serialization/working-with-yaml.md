---
title:                "Werken met YAML"
aliases: - /nl/elixir/working-with-yaml.md
date:                  2024-01-28T22:11:38.685590-07:00
model:                 gpt-4-0125-preview
simple_title:         "Werken met YAML"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/elixir/working-with-yaml.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?

Werken met YAML betekent het parsen en genereren van YAML-geformatteerde data, een voor mensen leesbare standaard voor data-serialisatie. Programmeurs doen dit voor configuratiebestanden, data-uitwisseling, en omdat het leesbaarder is dan JSON of XML voor complexe datastructuren.

## Hoe:

Elixir bevat geen ingebouwde YAML-ondersteuning, maar je kunt de `yamerl` bibliotheek gebruiken. Voeg eerst `yamerl` toe aan je `mix.exs` bestand:

```elixir
defp deps do
  [{:yamerl, "~> 0.8"}]
end
```

Na het uitvoeren van `mix deps.get`, kun je YAML parsen:

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

Dit zal uitvoeren:

```elixir
[
  %{
    "age" => 30,
    "langs" => ["Elixir", "Ruby", "Haskell"],
    "name" => "John Doe"
  }
]
```

En om Elixir data naar YAML om te zetten:

```elixir
data = %{
  name: "John Doe",
  age: 30,
  langs: ["Elixir", "Ruby", "Haskell"]
}

yml_string = :yamerl.encode(data)
IO.puts yml_string
```

Dit print:

```yaml
---
age: 30
langs:
  - Elixir
  - Ruby
  - Haskell
name: John Doe
```

## Diepgaande duik

YAML, wat staat voor "YAML Ain't Markup Language" (een recursief acroniem), bestaat sinds 2001. JSON en XML kunnen vergelijkbare doeleinden dienen, maar de focus van YAML op leesbaarheid maakt het populair voor configuraties. `yamerl`, een Erlang bibliotheek aangepast voor Elixir via interoperabiliteit, is een solide keuze voor Elixir-ontwikkelaars. Vergeet niet, YAML is gevoelig voor inspringing, wat het parsen een beetje lastiger maakt in vergeliking met JSON.

## Zie ook

- Officiële `yamerl` GitHub repository: https://github.com/yakaz/yamerl
- Elixir `hexdocs` voor YAML-bibs: https://hex.pm/packages?search=yaml&sort=recent_downloads
- Officiële YAML site voor specificaties en meer: https://yaml.org
- Elixir School voor het leren van Elixir: https://elixirschool.com/nl/
