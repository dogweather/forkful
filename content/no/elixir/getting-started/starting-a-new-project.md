---
date: 2024-01-20 18:03:24.852961-07:00
description: "Slik gj\xF8r du: ."
lastmod: '2024-03-13T22:44:40.444217-06:00'
model: gpt-4-1106-preview
summary: .
title: "\xC5 starte et nytt prosjekt"
weight: 1
---

## Slik gjør du:
```
# Installer Elixir hvis du ikke allerede har det
# Åpne terminalen og kjør:
mix new mitt_prosjekt

# Du vil se noe slik som dette:
* creating README.md
* creating .formatter.exs
* creating .gitignore
* creating mix.exs
* creating lib
* creating lib/mitt_prosjekt.ex
* creating test
* creating test/test_helper.exs
* creating test/mitt_prosjekt_test.exs

# Naviger inn i prosjektmappen:
cd mitt_prosjekt

# Start et interaktivt Elixir-shell med prosjektet ditt lastet:
iex -S mix
```

## Dypdykk
Å starte et nytt prosjekt har alltid vært en sentral del av utviklerens arbeidsflyt. I Elixir-verdenen er `mix` verktøyet for prosjektadministrasjon. Det ble utgitt sammen med Elixir og er inspirert av verktøy fra andre språk, som `bundler` fra Ruby. Alternativer som `rebar3` eksisterer i Erlang-økosystemet, men `mix` er spesielt tilpasset Elixirs behov. `mix` håndterer ikke bare prosjektoppstart, men også avhengighetsstyring og task-running. Strukturen som er opprettet er modulær, og fremmer god praksis for vedlikeholdbar kode.

## Se Også
- Elixir's offisielle dokumentasjon for `mix` på https://hexdocs.pm/mix/Mix.html
- Innføring i Elixir på https://elixir-lang.org/getting-started/introduction.html
- Prosjektstruktur og konvensjoner på https://hexdocs.pm/mix/Mix.html#module-project-structure
