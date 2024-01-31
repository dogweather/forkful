---
title:                "Arbeid med YAML"
date:                  2024-01-19
html_title:           "Arduino: Arbeid med YAML"
simple_title:         "Arbeid med YAML"

category:             "Elixir"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/elixir/working-with-yaml.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
YAML er et dataformat for serialisering av data, lik JSON og XML. Programmerere bruker YAML fordi det er lett å lese for mennesker, det spiller godt med komplekse datastrukturer og passer ypperlig for konfigurasjonsfiler.

## Slik gjør du:
For å jobbe med YAML i Elixir, trenger vi en YAML-bibliotek. `yamerl` er en populær ett. Først, legg til `yamerl` i `mix.exs`:

```elixir
defp deps do
  [
    {:yamerl, "~> 0.8.0"}
  ]
end
```

Kjør deretter `mix deps.get` for å hente den nye avhengigheten.

Når vi har `yamerl`, kan vi serialisere Elixir-strukturer til YAML og parse YAML til Elixir-strukturer. 

Slik parser du YAML-tekst:
```elixir
yaml_to_parse = """
name: John Doe
age: 30
list_of_hobbies:
  - programming
  - cycling
"""

{:ok, parsed_yaml} = :yamerl_constr.string(yaml_to_parse)
IO.inspect(parsed_yaml)
```

For å konvertere Elixir-struktur til YAML-string:
```elixir
data_to_convert = %{
  name: "Jane Smith",
  age: 25,
  list_of_hobbies: ["reading", "hiking"]
}

yaml_string = :yamerl_encode.encode(data_to_convert)
IO.puts(yaml_string)
```

Eksempeloutput:
```elixir
%{
  "age" => 30,
  "list_of_hobbies" => ["programming", "cycling"],
  "name" => "John Doe"
}
name: Jane Smith
age: 25
list_of_hobbies:
  - reading
  - hiking
```

## Dypdykk
YAML startet i 2001, inspirert av XML og JSON. YAML står for "YAML Ain't Markup Language" og er laget for å være enkel å forstå. Alternativer som JSON og TOML finnes, men YAML er fortsatt foretrukket i mange tilfeller, spesielt for konfigurasjonsfiler. I Elixir, håndterer YAML-biblioteker parsing gjennom Erlang-biblioteker som `yamerl`. Disse bibliotekene gjør Erlang/Elixir-kompatibel manipulasjon av YAML-data mulig.

## Se Også
- YAML offisielle side: https://yaml.org
- `yamerl` biblioteket på Hex: https://hex.pm/packages/yamerl 
- Elixir's offisielle dokumentasjon: https://elixir-lang.org/docs.html 
- JSON i Elixir: https://hexdocs.pm/poison/readme.html
- TOML i Elixir: https://hex.pm/packages/toml
