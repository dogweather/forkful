---
date: 2024-01-26 03:48:17.287774-07:00
description: "Hvordan: Elixir leveres med en innebygd grafisk feils\xF8ker kalt `:debugger`.\
  \ For \xE5 bruke den, m\xE5 du starte den og koble til din kj\xF8rende prosess.\
  \ F\xF8rst,\u2026"
lastmod: '2024-03-13T22:44:40.447872-06:00'
model: gpt-4-0125-preview
summary: "Elixir leveres med en innebygd grafisk feils\xF8ker kalt `:debugger`."
title: "\xC5 bruke en feils\xF8ker"
weight: 35
---

## Hvordan:
Elixir leveres med en innebygd grafisk feilsøker kalt `:debugger`. For å bruke den, må du starte den og koble til din kjørende prosess.

Først, sørg for at du har startet `:debugger` innenfor en `iex`-økt:
```elixir
iex> :debugger.start()
{:ok, #PID<0.108.0>}
```

Nå, tolk modulen du ønsker å feilsøke:
```elixir
iex> :int.ni(MyApp.MyModule)
{:module, MyApp.MyModule}
```

Du kan sette et brytepunkt:
```elixir
iex> :int.break(MyApp.MyModule, linjenummer)
:ok
```

Og deretter, kjøre funksjonen din for å treffe brytepunktet og stegge gjennom koden din:
```elixir
iex> MyApp.MyModule.min_funksjon(arg1, arg2)
# Debugger vil pause utførelsen ved linjen med brytepunktet
```

## Dypdykk
Før Elixirs `:debugger`, tilbød Erlang debuggeren som Elixir bruker; den er robust og flink til å håndtere samtidige prosesser, et sterkt punkt av Erlang VM (BEAM). I motsetning til noen andre debuggere, lar ikke `:debugger` deg modifisere variabler på fly, på grunn av den umutable naturen til data i Elixir. Når det gjelder alternativer, har du `IEx.pry` som lar deg pause utførelsen og hoppe inn i en REPL hvor som helst i koden din, noe som kan være super hendig.

Mens `:debugger` er god for et grafisk grensesnitt, kan noen foretrekke det innebygde verktøyet `:observer` som også tilbyr inspeksjon av prosesser og systemmetrikker, selv om det ikke er spesifikt rettet mot å stegge gjennom kode. Elixirs fellesskap bidrar også med verktøy som `visualixir` og `rexbug`, som utvider økosystemet av feilsøkingsverktøy utover standardene.

## Se også
- Offisiell Elixir Getting Started Guide om Debugging: https://elixir-lang.org/getting-started/debugging.html
- Erlangs `:debugger` Dokumentasjon: http://erlang.org/doc/apps/debugger/debugger_chapter.html
- Elixir Forum Diskusjoner om Feilsøkingsteknikker: https://elixirforum.com/c/elixir-questions/elixir-questions-questions-help/15
