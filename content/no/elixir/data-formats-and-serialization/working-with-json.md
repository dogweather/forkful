---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:22:10.149634-07:00
description: "\xC5 jobbe med JSON inneb\xE6rer \xE5 analysere JSON-formaterte strenger\
  \ til datastrukturer som Elixir kan manipulere, og serialisere Elixir-datastrukturer\
  \ tilbake\u2026"
lastmod: '2024-02-25T18:49:38.691450-07:00'
model: gpt-4-0125-preview
summary: "\xC5 jobbe med JSON inneb\xE6rer \xE5 analysere JSON-formaterte strenger\
  \ til datastrukturer som Elixir kan manipulere, og serialisere Elixir-datastrukturer\
  \ tilbake\u2026"
title: Arbeider med JSON
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Å jobbe med JSON innebærer å analysere JSON-formaterte strenger til datastrukturer som Elixir kan manipulere, og serialisere Elixir-datastrukturer tilbake til JSON-strenger. Dette er essensielt for webutvikling, API-er og konfigurasjonsfiler, siden JSON er et lettvekts, tekstbasert, språkuavhengig datautvekslingsformat som er mye brukt for sin enkelhet og menneskelesbarhet.

## Hvordan:

I Elixir kan du bruke `Jason`-biblioteket, et populært valg for JSON-analysering og generering. Først, legg til `Jason` i prosjektets avhengigheter i `mix.exs`:

```elixir
defp deps do
  [
    {:jason, "~> 1.3"}
  ]
end
```

Deretter kjører du `mix deps.get` for å hente avhengigheten.

### Parse JSON:
For å konvertere en JSON-streng til Elixir-datastrukturer:

```elixir
json_string = "{\"name\":\"John\", \"age\":30}"
{:ok, person} = Jason.decode(json_string)
IO.inspect(person)
# Utdata: %{"name" => "John", "age" => 30}
```

### Generere JSON:
For å konvertere en Elixir-map til en JSON-streng:

```elixir
person_map = %{"name" => "Jane", "age" => 25}
{:ok, json_string} = Jason.encode(person_map)
IO.puts(json_string)
# Utdata: {"age":25,"name":"Jane"}
```

### Arbeide med Structs:
For å kode en Elixir-struct, må du implementere `Jason.Encoder`-protokollen for din struct. Her er et eksempel:

```elixir
defmodule Person do
  @derive {Jason.Encoder, only: [:name, :age]}
  defstruct name: nil, age: nil
end

person_struct = %Person{name: "Mike", age: 28}
{:ok, json_string} = Jason.encode(person_struct)
IO.puts(json_string)
# Utdata: {"age":28,"name":"Mike"}
```

Denne enkle tilnærmingen vil hjelpe deg å komme i gang med å integrere JSON-behandling i dine Elixir-applikasjoner, og lette datautveksling i ulike programmeringsmiljøer.
