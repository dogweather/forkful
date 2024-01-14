---
title:                "Elixir: Søking og erstatting av tekst"
simple_title:         "Søking og erstatting av tekst"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/elixir/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Hvorfor

Å søke og erstatte tekst er en vanlig oppgave innen programmering. Dette kan være nødvendig for å endre variabelnavn, fjerne feilaktige karakterer eller bare en generell renseprosess av kode. Elixir, et funksjonelt programmeringsspråk, tilbyr kraftige verktøy for å utføre disse oppgavene på en enkel og effektiv måte.

## Hvordan

I Elixir bruker vi funksjonen `String.replace/4` for å søke og erstatte tekst. Denne funksjonen tar fire argumenter: den originale teksten, søkeordet, erstatningsordet og en liste med modifikasjonstegn. La oss se på et eksempel:

```elixir
original_tekst = "God dag, verden!"
søkeord = "dag"
erstatningsord = "kveld"

ny_tekst = String.replace(original_tekst, søkeord, erstatningsord, ["c"])
IO.puts ny_tekst  # Resultatet blir "God kveld, verden!"
```

I dette eksempelet har vi byttet ut "dag" med "kveld", og vi har også inkludert modifikasjonstegnet "c", som står for "case-sensitive" (så ordet "Dag" ville ikke blitt byttet ut).

Det er også mulig å bruke regulære uttrykk ved hjelp av `Regex.replace/3`-funksjonen. Dette er nyttig når du trenger å søke og erstatte mer komplekse mønstre i teksten. Her er et eksempel på å erstatte alle tall med "X":

```elixir
tekst = "Det er 123 mangoer på bordet"
ny_tekst = Regex.replace(~r/\d+/, tekst, "X")
IO.puts ny_tekst # Resultatet blir "Det er X mangoer på bordet"
```

## Dypdykk

Begge disse funksjonene gir deg muligheten til å spesifisere hvor mange ganger du vil gjøre søket og erstatningen. Du kan også bruke `String.replace_all/4` og `Regex.replace_all/3` for å utføre søk og erstatt operasjoner på tekststrenger hvor søket skjer flere ganger.

Det er også verdt å nevne at både `String.replace/4` og `Regex.replace/3` returnerer en kopi av den originale teksten med søket og erstatningen gjort. Dette betyr at den originale teksten ikke blir endret, og du må lagre det returnerte resultatet hvis du ønsker å bruke den endrede teksten videre.

## Se Også

- [Elixir Docs om String.replace/4](https://hexdocs.pm/elixir/String.html#replace/4)
- [Elixir Docs om Regex.replace/3](https://hexdocs.pm/elixir/Regex.html#replace/3)
- [Elixir offisiell hjemmeside](https://elixir-lang.org/)