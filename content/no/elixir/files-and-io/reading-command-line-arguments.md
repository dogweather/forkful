---
date: 2024-01-20 17:56:07.895007-07:00
description: "How To (Hvordan) I de tidlige dagene av datamaskiner ga kommandolinjeinteraksjon\
  \ programmerere en direkte linje til operativsystemet. I Elixir, som er en\u2026"
lastmod: '2024-04-05T21:53:41.437880-06:00'
model: gpt-4-1106-preview
summary: "I\_Elixir, som er en moderne spr\xE5k, f\xE5r vi tilgang til kommandolinjeargumenter\
  \ med `System.argv()`."
title: Lese kommandolinjeargumenter
weight: 23
---

## How To (Hvordan)
```Elixir
defmodule Greeter do
  def main(args) do
    case args do
      [name] ->
        IO.puts("Hei, #{name}!")
      _ ->
        IO.puts("Heisann! Hvem er du?")
    end
  end
end

# Hvis lagret som greeter.exs, kjør dette i terminalen:
# elixir greeter.exs Odin
# Output: Hei, Odin!
```

## Deep Dive (Dypdykk)
I de tidlige dagene av datamaskiner ga kommandolinjeinteraksjon programmerere en direkte linje til operativsystemet. I Elixir, som er en moderne språk, får vi tilgang til kommandolinjeargumenter med `System.argv()`. Alternativer inkluderer bruk av OptionParser-modulen for mer komplekse behov der du kan tolke flagg og nøkkel/verdi-argumenter. Under panseret konverterer BEAM-vm, som Elixir kjører på, brukerinput fra en binærstreng til Elixir-strenger og lister som vi enkelt kan manipulere.

## See Also (Se Også)
- [Elixir-lang.org - Getting Started Guide](https://elixir-lang.org/getting-started/mix-otp/introduction-to-mix.html)
- [HexDocs - OptionParser](https://hexdocs.pm/elixir/OptionParser.html)
