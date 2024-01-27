---
title:                "Sette streng til store bokstaver"
date:                  2024-01-19
html_title:           "Arduino: Sette streng til store bokstaver"
simple_title:         "Sette streng til store bokstaver"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/elixir/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Å kapitalisere en streng betyr å gjøre første bokstav i ord til en stor bokstav. Programmerere gjør dette for lesbarhet og for å møte tekstformatkrav.

## Hvordan gjøre det:
```elixir
defmodule StringHelpers do
  def capitalize_string(str) do
    String.capitalize(str)
  end
end

IO.puts StringHelpers.capitalize_string("hei, elixir fans!")  # Output: "Hei, elixir fans!"

```

## Deep Dive
Kapitalisering av strenger har eksistert siden typemaskintiden. Det hjelper med å merke begynnelsen på setninger og egennavn, noe som gjør teksten mer tiltalende visuelt. I Elixir, `String.capitalize/1` er den innebygde funksjonen som gjør dette; den kapitaliserer bare det første tegnet og gjør resten av strengen liten. Det finnes alternativer som `titleize` fra `elixir_titleize` biblioteket som kapitaliserer første bokstav i hvert ord.

Implementeringsdetaljer kan variere. `String.capitalize/1` i Elixir håndterer unicode og lar ikke bare ASCII-tegn bruke den. Det er smart nok til å kjenne igjen små og store unicode-tegn og gjøre endringer deretter.

## Se Også
- Elixir's offisielle dokumentasjon for String-modulen: [https://hexdocs.pm/elixir/String.html](https://hexdocs.pm/elixir/String.html)
