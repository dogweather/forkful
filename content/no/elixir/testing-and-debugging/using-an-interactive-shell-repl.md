---
date: 2024-01-26 04:13:25.883401-07:00
description: "Hvordan: For \xE5 starte IEx, \xE5pne terminalen din og skriv `iex`.\
  \ Her er en smakebit."
lastmod: '2024-03-13T22:44:40.445075-06:00'
model: gpt-4-0125-preview
summary: "For \xE5 starte IEx, \xE5pne terminalen din og skriv `iex`."
title: Bruke et interaktivt skall (REPL)
weight: 34
---

## Hvordan:
For å starte IEx, åpne terminalen din og skriv `iex`. Her er en smakebit:

```Elixir
iex> name = "Elixir Programmer"
"Elixir Programmer"
iex> String.length(name)
17
iex> Enum.map([1, 2, 3], fn num -> num * 3 end)
[3, 6, 9]
```

Resultatet skal vise variabeltildeling, funksjonsresultater, og en anonym funksjon i arbeid.

## Dypdykk
IEx-skalen har vært en del av Elixir siden dens første dager. José Valim, skaperen av Elixir, hentet inspirasjon fra de interaktive skalene fra andre språk som Pythons `python` og Rubys `irb`. Mens IEx deler mange funksjoner med disse, er den bygget for å håndtere Elixirs samtidige natur og er fullt integrert med Erlang VMs kapasiteter.

Alternativer til IEx i Erlang-økosystemet inkluderer `erl`, Erlang-skallen. Men IEx gir et mer Elixir-vennlig miljø, med funksjoner som omfattende tabulatorkompletering, historie og hjelpere.

IEx REPL er mer enn en lekeplass; den kan sømløst koble til et kjørende system. Dette er avgjørende for feilsøking av live-applikasjoner. Den underliggende implementeringen støtter seg på BEAM (Erlang VM), som sikrer at funksjoner som varm kodebytte støttes rett i skallet.

## Se Også
Sjekk disse ut for ytterligere lesing og ressurser:

- [Elixirs IEx-dokumentasjon](https://hexdocs.pm/iex/IEx.html)
- [Interaktiv Elixir (IEx) - Elixir-skallet](https://elixir-lang.org/getting-started/introduction.html#interactive-elixir)
- [Erlangs `erl`-dokumentasjon](http://erlang.org/doc/man/erl.html)
- [Å lære Elixirs Interaktive Skall](https://elixirschool.com/en/lessons/basics/iex_helpers/)
