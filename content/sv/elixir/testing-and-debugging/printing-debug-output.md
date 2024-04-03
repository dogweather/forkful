---
date: 2024-01-20 17:52:30.326612-07:00
description: "Hur g\xF6r man: Elixir ger oss `IO.inspect/2` f\xF6r att enkelt printa\
  \ ut variabler och se deras v\xE4rde."
lastmod: '2024-03-13T22:44:37.569685-06:00'
model: gpt-4-1106-preview
summary: "Elixir ger oss `IO.inspect/2` f\xF6r att enkelt printa ut variabler och\
  \ se deras v\xE4rde."
title: "Skriva ut fels\xF6kningsdata"
weight: 33
---

## Hur gör man:
Elixir ger oss `IO.inspect/2` för att enkelt printa ut variabler och se deras värde. 

```elixir
defmodule MyModule do
  def my_function(data) do
    data
    |> process_data()
    |> IO.inspect(label: "Efter bearbetning")
  end

  defp process_data(data) do
    # Din logik här
  end
end
```
Output:
```
Efter bearbetning: [din bearbetade data här]
```

För enklare meddelanden används `IO.puts/1` eller `IO.write/1`:

```elixir
IO.puts("Skriver ut ett enkelt meddelande")
```

## Djupdykning
Utskrift av debug-information är inte unikt för Elixir; det härstammar från de tidigaste dagarna av programmering. Alternativ i Elixir inkluderar Logger-modulen som erbjuder olika nivåer av loggmeddelanden. Implementationen av `IO.inspect/2` i Elixir är direkt och effektiv: den returnerar datan efter att ha skrivit den, vilket gör den enkel att kedja i pipeline.

## Se också
- [Elixir `IO` Module Documentation](https://hexdocs.pm/elixir/IO.html)
- [Elixir `Logger` Module Documentation](https://hexdocs.pm/logger/Logger.html)
- [Erlang's `dbg` Module for Tracing](http://erlang.org/doc/man/dbg.html)
