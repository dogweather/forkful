---
date: 2024-01-20 17:55:44.604382-07:00
description: "L\xE4sa kommandoradsargument inneb\xE4r att f\xE5nga de v\xE4rden som\
  \ anv\xE4ndaren matar in n\xE4r de k\xF6r ditt program. Programmerare g\xF6r detta\
  \ f\xF6r att till\xE5ta dynamisk\u2026"
lastmod: 2024-02-19 22:04:56.829434
model: gpt-4-1106-preview
summary: "L\xE4sa kommandoradsargument inneb\xE4r att f\xE5nga de v\xE4rden som anv\xE4\
  ndaren matar in n\xE4r de k\xF6r ditt program. Programmerare g\xF6r detta f\xF6\
  r att till\xE5ta dynamisk\u2026"
title: "L\xE4sa in kommandoradsargument"
---

{{< edit_this_page >}}

## Vad & Varför?
Läsa kommandoradsargument innebär att fånga de värden som användaren matar in när de kör ditt program. Programmerare gör detta för att tillåta dynamisk input och anpassa programmets beteende vid körning.

## Så här gör du:
Elixir gör det enkelt att hantera kommandoradsargument med `System.argv/0`. Här är ett exempel:

```elixir
defmodule Greeter do
  def main(args) do
    args |> Enum.join(" ") |> IO.puts()
  end
end

Greeter.main(System.argv())
```
Om du sparar detta som `greeter.exs` och kör `elixir greeter.exs Hej Värld`, blir output:
```
Hej Värld
```

## Fördjupning
Elixir, som är byggt på Erlang's VM, ärande för skriptspråk med lätt skalbarhet och felhantering. Att läsa in kommandoradsargument är vanligt i många språk och verktyg för kommando rader som `optparse` finns också i Elixir för mer komplexa ändamål.

Din kod kör i Elixir's interpreter, där `System.argv/0` är ett inbyggt anrop som ger tillgång till användardefinierade argument. För mer control, använd `OptionParser.parse/2` som hanterar flaggor och nyckelord etc.

Ett exempel med `OptionParser`:

```elixir
defmodule Greeter do
  def main do
    {opts, args, _} = OptionParser.parse(System.argv())
  
    welcome = if opts[:capitalize], do: String.upcase(Enum.join(args, " ")), else: Enum.join(args, " ")
  
    IO.puts welcome
  end
end

Greeter.main()
```
Om du kör `elixir greeter.exs --capitalize hej värld`, blir output `HEJ VÄRLD`.

## Se även:
- [Elixir's System module dokumentation](https://hexdocs.pm/elixir/System.html)
- [Elixir's OptionParser module dokumentation](https://hexdocs.pm/elixir/OptionParser.html)
- [Creational Patterns for Erlang/Elixir: Using Command Line Arguments](https://medium.com/erlang-central/creational-patterns-for-erlang-elixir-using-command-line-arguments-8404fecd4a79)
