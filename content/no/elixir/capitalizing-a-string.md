---
title:                "Sette en streng i store bokstaver"
html_title:           "Elixir: Sette en streng i store bokstaver"
simple_title:         "Sette en streng i store bokstaver"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/elixir/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Å forstørre en streng betyr å gjøre den første bokstaven i hver ord i strengen til en stor bokstav. Programmerere gjør dette for å forbedre lesbarheten og presentasjonen av tekstdata.

## Hvordan: 

Vi bruker `String.capitalize/2` funksjonen i Elixir:

```elixir
IO.puts String.capitalize("elixir programmering")
```

Output:
  
```elixir 
"Elixir Programmering"
```

## Dypdykk: 

Historisk sett handler strengkapitalisering om typografiske konvensjoner, hvor stor forbokstav brukes for egennavn, setningsstarting, titler osv.

Alternativer til `String.capitalize/2` i Elixir kan være direkte manipulering av strenger, men dette kan være mer komplisert og feilutsatt. `String.capitalize/2` tilbyr en pålitelig og enkel løsning.

Ved en nærmere titt, `String.capitalize/2` funksjonen bruker Unicode datatabeller og regler for å transformere strenger. Derfor fungerer det på tvers av forskjellige språk og skrifter, noe som sikrer pålitelighet og omfattende støtte.

## Se også: 

For detaljert informasjon og ytterligere lesning, se følgende ressurser:

- Elixir offisiell dokumentasjon på `String.capitalize/2` [her](https://hexdocs.pm/elixir/String.html#capitalize/2)


- For en bredere forståelse av hvordan Elixir håndterer strenger og Unicode, se [dette](https://elixir-lang.org/getting-started/binaries-strings-and-char-lists.html).