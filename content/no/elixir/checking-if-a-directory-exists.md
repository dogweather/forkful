---
title:                "Elixir: Sjekke om en mappe eksisterer"
simple_title:         "Sjekke om en mappe eksisterer"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/elixir/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

# Hvorfor

Det er viktig å vite om en mappe eksisterer når man jobber med programmering, spesielt hvis man trenger å hente ut eller lagre data. Ved å sjekke om en mappe eksisterer, kan man unngå feil og sikre at programmet fungerer som forventet.

# Slik gjør du det

For å sjekke om en mappe eksisterer, kan du bruke funksjonen `File.dir?/1`, hvor 1 representerer stien til mappen du ønsker å sjekke. Her er et eksempel på hvordan du bruker denne funksjonen i praksis:

```elixir
resultat = File.dir?("/brukere/brukernavn/mappenavn")
IO.puts resultat

# output: true
```

I dette tilfellet vil funksjonen returnere `true` hvis mappen eksisterer, og `false` hvis den ikke gjør det.

# Dypdykk

Hvis du ønsker å lære mer om hvordan sjekking av mapper fungerer i Elixir, kan du se på hvordan funksjonen `File.dir?/1` er implementert i Elixir-kildekoden. Du kan også utforske ulike metoder for å håndtere forskjellige typer mapper, for eksempel skjulte mapper eller mapper med spesielle tillatelser.

# Se også

- Offisiell Elixir dokumentasjon for `File.dir?/1`: https://hexdocs.pm/elixir/File.html#dir?/1
- Artikkel om filbehandling i Elixir: https://elixirschool.com/nn/lessons/specifics/file/