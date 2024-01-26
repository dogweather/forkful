---
title:                "Arbeta med JSON"
html_title:           "Arduino: Arbeta med JSON"
simple_title:         "Arbeta med JSON"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/elixir/working-with-json.md"
---

{{< edit_this_page >}}

## Vad och varför?
JSON står för JavaScript Object Notation och används för att lagra och utbyta data. Programmerare hanterar JSON för att enkelt interagera med webb-API:er och lagra strukturerad information.

## Hur gör man:
Elixir använder biblioteket `Jason` för att hantera JSON. Här är några exempel:

```elixir
# Lägg till Jason i dina beroenden i mix.exs fil
defp deps do
  [
    {:jason, "~> 1.2"}
  ]
end

# Exempel på hur man avkodar en JSON-sträng
iex> Jason.decode("{\"key\": \"value\"}")
{:ok, %{"key" => "value"}}

# Exempel på hur man kodar Elixir datastruktur till en JSON-sträng
iex> Jason.encode(%{"key" => "value"})
{:ok, "{\"key\":\"value\"}"}
```

## Djupdykning:
JSON är en textbaserad datautbytesstandard som blev populär tack vare dess enkelhet och läsbarhet. Alternativ till JSON inkluderar XML och YAML, men JSON har blivit standard för webb-API:er. I Elixir kan `Jason` hantera de flesta behov, men `Poison` är ett annat alternativ. `Jason` integreras smidigt med populära webbramverk som Phoenix och är effektiv tack vare Elixir's concurrent natur.

## Se även:
- Jason-dokumentation: https://hexdocs.pm/jason/readme.html
- Elixir School JSON-lektioner: https://elixirschool.com/en/lessons/specifics/jason/
- ElixirForum för diskussioner: https://elixirforum.com
