---
title:                "Arbete med YAML"
date:                  2024-01-19
simple_title:         "Arbete med YAML"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/elixir/working-with-yaml.md"
---

{{< edit_this_page >}}

## Vad & Varför?
YAML är ett dataformat för att strukturera information, liknande JSON men mer läsbart för människor. Programmerare använder YAML för konfigurationsfiler, datautbyte och att definiera datastrukturer på ett enkelt sätt.

## Hur gör man:
För att hantera YAML i Elixir behöver du ett bibliotek som `yamerl`. Lägg till den som beroende i din `mix.exs` och kör `mix deps.get` för att installera.

```elixir
# Lägg till yamerl i mix.exs
defp deps do
  [
    {:yamerl, "~> 0.8"}
  ]
end
```

Läs en YAML-fil och omvandla till Elixir datastruktur:

```elixir
# Använda Yamerl för att läsa YAML
{:ok, yamerl} = Application.ensure_all_started(:yamerl)

yaml_content = """
- cat
- dog
- mouse
"""

{:ok, data} = :yamerl_constr.string(yaml_content)
IO.inspect(data) # Skriver ut: [cat, dog, mouse]
```

Skriva en Elixir datastruktur till en YAML-sträng:

```elixir
# Skapa en YAML-sträng från Elixir
list = ["cat", "dog", "mouse"]
yaml_string = :yamerl.encode(list)
IO.puts(yaml_string)
# Output:
# - cat
# - dog
# - mouse
```

## Deep Dive
YAML (YAML Ain't Markup Language) skapades 2001 för att vara användarvänligt och lätt att förstå. Alternativ som JSON och XML används också för datarepresentation, men YAML är populär bland utvecklare för sin tydlighet och enkelhet. Elixirs YAML-hantering bygger på Erlang-biblioteket `yamerl`, vilket gör det enkelt att infoga YAML-stöd i Elixir-program.

## Se Också
- YAML officiell webbplats: [https://yaml.org](https://yaml.org)
- `yamerl` GitHub sida: [https://github.com/yakaz/yamerl](https://github.com/yakaz/yamerl)
- Elixir officiell dokumentation: [https://hexdocs.pm/elixir/](https://hexdocs.pm/elixir/)
- Elixir Forum för diskussioner: [https://elixirforum.com](https://elixirforum.com)
