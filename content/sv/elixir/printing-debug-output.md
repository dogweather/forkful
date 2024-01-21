---
title:                "Skriva ut felsökningsdata"
date:                  2024-01-20T17:52:30.326612-07:00
model:                 gpt-4-1106-preview
simple_title:         "Skriva ut felsökningsdata"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/elixir/printing-debug-output.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att skriva ut debug-information i Elixir hjälper utvecklare att förstå vad deras kod gör i realtid och varför den beter sig på ett visst sätt. Det är en ovärderlig teknik för att spåra och fixa buggar.

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