---
title:                "Lese kommandolinjeargumenter"
aliases: - /no/elixir/reading-command-line-arguments.md
date:                  2024-01-20T17:56:07.895007-07:00
model:                 gpt-4-1106-preview
simple_title:         "Lese kommandolinjeargumenter"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/elixir/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## What & Why? (Hva & Hvorfor?)
Å lese kommandolinjeargumenter handler om å hente data som brukeren gir når de kjører programmet ditt. Vi gjør det for å gjøre programmer fleksible – slik at de kan utføre forskjellige oppgaver basert på hva brukeren ønsker.

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
