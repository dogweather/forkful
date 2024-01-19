---
title:                "Lese kommandolinjeargumenter"
html_title:           "Arduino: Lese kommandolinjeargumenter"
simple_title:         "Lese kommandolinjeargumenter"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/elixir/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Når vi snakker om å lese kommandolinje-argumenter, mener vi å ta innspill fra brukeren direkte fra kommandolinjen mens de kjører et program. Dette gir programmerere større fleksibilitet, slik at de kan lage mer dynamiske og tilpassbare programmer.

## How to:
Bruke `System.argv/0` funksjonen i Elixir lar deg gripe og håndtere kommandolinje-argumenter. Her er en enkel bruk:

```Elixir
defmodule Test do
  def main(args) do
    IO.inspect(args)
  end
end

System.argv(["-e","Test.main([])"])
```

Når du kjører dette, vil du se at det blir printet en liste med strengene "-e" og "Test.main([])" til terminalen.

## Deep Dive
Lesing av kommandolinjeargumenter var en nødvendighet helt tilbake til de tidligste operativsystemene. Spesifikt for Elixir, kan du også bruke funksjonen `OptionParser.parse/2` for mer kompliserte inngangstolkninger. Dette er spesielt nyttig hvis du trenger å håndtere flagg eller nøkkel-verdi par. På implementasjonsnivå blir disse argumentene lagt til i en liste og passert til hovedfunksjonen ved oppstart.

## See Also
For mer informasjon om hvordan du bruker `System.argv/0` og `OptionParser.parse/2`, se den offisielle Elixir-dokumentasjonen:
- [System.argv/0](https://hexdocs.pm/elixir/System.html#argv/0)
- [OptionParser.parse/2](https://hexdocs.pm/elixir/OptionParser.html#parse/2)