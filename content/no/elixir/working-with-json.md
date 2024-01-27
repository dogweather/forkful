---
title:                "Arbeid med JSON"
date:                  2024-01-19
html_title:           "Arduino: Arbeid med JSON"
simple_title:         "Arbeid med JSON"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/elixir/working-with-json.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Jobber med JSON innebærer å parse (gjøre om) og generere JSON (JavaScript Object Notation) data, et lett-å-bruke dataformat. Programmerere bruker JSON fordi det er enkelt å lese og skrive for mennesker, og lett å parse og generere for maskiner, noe som gjør det ideelt for datautveksling.

## Hvordan:

Elixir bruker ofte `Jason`, et pakke for JSON manipulasjon. Her er rask guide:

For å starte, legg `jason` til din `mix.exs`:

```elixir
defp deps do
  [
    {:jason, "~> 1.2"}
  ]
end
```

Kjør `mix deps.get` for å installere.

Parse en JSON-streng:

```elixir
{:ok, data} = Jason.decode("{\"key\": \"value\"}")
IO.inspect(data) # => %{"key" => "value"}
```

Lage en JSON-streng fra Elixir data:

```elixir
json_string = Jason.encode!(%{"key" => "value"})
IO.puts(json_string) # => {"key":"value"}
```

## Dypdykk:

Historisk sett, overtok JSON popularitet fra XML på grunn av sin enkelhet. Noen alternativer inkluderer `Poison` og `Oj` for Elixir, men `Jason` er kjent for sin hastighet og enkelhet.

Implementeringsdetaljer: `Jason` konverterer data mellom Elixir-strukturer og JSON-strenger. Det er nøkkelfokusert på ytelse og sikkerhet, så det kan trygt brukes selv med uventet input.

## Se Også:

- Elixir's offisielle dokumentasjon for `Jason`: https://hexdocs.pm/jason/Jason.html
- JSON spesifikasjon: https://www.json.org/json-en.html
- Introduksjon til Elixir: https://elixir-lang.org/getting-started/introduction.html
- Elixir Forum for diskusjoner relateret til `Jason`: https://elixirforum.com
