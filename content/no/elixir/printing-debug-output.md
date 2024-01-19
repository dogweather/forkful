---
title:                "Utskrift av feilsøkingsresultat"
html_title:           "Arduino: Utskrift av feilsøkingsresultat"
simple_title:         "Utskrift av feilsøkingsresultat"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/elixir/printing-debug-output.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Utskrift av debug-informasjon er en måte å inspisere data på kjøretid i programmeringskoden. Løse feil og forstå betydningen av komplekse datastrukturer blir mer håndterbart med det.

## Hvordan gjøre det:

Vi bruker `IO.inspect` funksjonen i Elixir for å skrive ut debug-informasjon på standard output:

```Elixir
defmodule Debug do
  def print_data do
    data = [1, 2, 3]
    
    IO.inspect(data)
  end
end

Debug.print_data()
```

Output ville være:
```Elixir
[1, 2, 3]
```

## Dypdykk

**Historisk sammenheng**: I eldre tid, innen sofistikert debugging-verktøy kom på plass, var utskrift av debug-informasjon den eneste måten å forstå hva som skjedde inni et kjørende program.

**Alternativer**: `IO.inspect` er ikke det eneste alternativet i Elixir. `IO.puts` og `IO.write` kan også benyttes til debugging formål, men de gir mindre detaljer.

**Implementeringsdetaljer**: `IO.inspect` kan også ta opsjoner som argumenter for å kontrollere detaljnivået av output. For eksempel,

```Elixir
IO.inspect(data, limit: :infinity)
```

vil skrive ut alle elementene i en stor liste eller tuple, i stedet for å bare skrive ut forhåndsinnstilte antall første elementer.

## Se også 

- Elixir offisiell dokumentasjon på IO-modulen: https://hexdocs.pm/elixir/IO.html
- Elixir skole tutorial på IO og skrive ut: https://elixirschool.com/en/lessons/basics/io/