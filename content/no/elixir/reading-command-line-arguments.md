---
title:                "Elixir: Lese kommandolinjeargumenter"
simple_title:         "Lese kommandolinjeargumenter"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/elixir/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Hvorfor?
Lesing av kommandolinje-argumenter er en nyttig ferdighet i Elixir-programmering. Det gjør det mulig for utviklere å ta imot brukerinput og behandle den på en dynamisk måte. Det er også en viktig del av å utvikle kommandolinjeverktøy og automatiseringsskript.

## Slik gjør du det
Det er enkelt å lese kommandolinje-argumenter i Elixir ved å bruke `System.argv/0` eller `System.argv/1` funksjoner. La oss se på et eksempel på hvordan dette kan gjøres:

```
defmodule CommandLine do
  def argv_example() do
    args = System.argv

    IO.puts "Antall argumenter: #{length(args)}"
    IO.puts "Kommandolinje-argumenter: #{inspect(args)}"
  end
end

CommandLine.argv_example()
```

Kjøring av denne koden med følgende kommandolinje-argumenter `elixir my_script.exs argument1 argument2` vil vise følgende output:

```
Antall argumenter: 3
Kommandolinje-argumenter: ["my_script.exs", "argument1", "argument2"]
```

Som du kan se, blir argumentene levert som en liste som kan manipuleres og brukes i programmet ditt.

## Dykk dypere
Å bruke `System.argv/0` vil gi deg en liste over alle kommandolinje-argumentene som er passert inn i programmet ditt, inkludert navnet på Elixir-scriptet som ble kjørt. Hvis du bare ønsker å få tilgang til argumentene som kommer etter navnet på scriptet, kan du bruke `System.argv/1` istedenfor. Dette vil gi deg en liste over argumentene uten navnet på scriptet.

En annen nyttig funksjon å kjenne til er `System.get_env/1` som lar deg få tilgang til miljøvariabler som er satt når programmet ditt kjører fra kommandolinjen.

## Se også
- [Dokumentasjon for `System.argv/0` og `System.argv/1` i Elixir](https://hexdocs.pm/elixir/System.html#argv/0)
- [Dokumentasjon for `System.get_env/1` i Elixir](https://hexdocs.pm/elixir/System.html#get_env/1)