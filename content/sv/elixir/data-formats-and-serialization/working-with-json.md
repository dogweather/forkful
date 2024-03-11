---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:22:06.928922-07:00
description: "Att arbeta med JSON inneb\xE4r att tolka JSON-formaterade str\xE4ngar\
  \ till datastrukturer som Elixir kan manipulera, och serialisera Elixir datastrukturer\u2026"
lastmod: '2024-03-11T00:14:10.920720-06:00'
model: gpt-4-0125-preview
summary: "Att arbeta med JSON inneb\xE4r att tolka JSON-formaterade str\xE4ngar till\
  \ datastrukturer som Elixir kan manipulera, och serialisera Elixir datastrukturer\u2026"
title: Arbeta med JSON
---

{{< edit_this_page >}}

## Vad och varför?

Att arbeta med JSON innebär att tolka JSON-formaterade strängar till datastrukturer som Elixir kan manipulera, och serialisera Elixir datastrukturer tillbaka till JSON-strängar. Detta är avgörande för webbutveckling, API:er, och konfigurationsfiler, eftersom JSON är ett lättviktigt, textbaserat, språkoberoende datautbytesformat som är brett använt för sin enkelhet och läsbarhet för människor.

## Hur man gör:

I Elixir kan du använda biblioteket `Jason`, ett populärt val för JSON-tolkning och generering. Lägg först till `Jason` i ditt projekts beroenden i `mix.exs`:

```elixir
defp deps do
  [
    {:jason, "~> 1.3"}
  ]
end
```

Kör sedan `mix deps.get` för att hämta beroendet.

### Tolka JSON:
För att konvertera en JSON-sträng till Elixir datastrukturer:

```elixir
json_string = "{\"name\":\"John\", \"age\":30}"
{:ok, person} = Jason.decode(json_string)
IO.inspect(person)
# Utdata: %{"name" => "John", "age" => 30}
```

### Generera JSON:
För att konvertera en Elixir-map till en JSON-sträng:

```elixir
person_map = %{"name" => "Jane", "age" => 25}
{:ok, json_string} = Jason.encode(person_map)
IO.puts(json_string)
# Utdata: {"age":25,"name":"Jane"}
```

### Arbeta med Strukturer:
För att koda en Elixir-struktur måste du implementera protokollet `Jason.Encoder` för din struktur. Här är ett exempel:

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

Denna enkla metod kommer att hjälpa dig att komma igång med att integrera JSON-behandling i dina Elixir-applikationer, vilket underlättar datadelning i olika programmeringsmiljöer.
