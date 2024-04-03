---
date: 2024-01-26 04:20:53.540368-07:00
description: "\xC5 jobbe med TOML betyr \xE5 analysere og generere TOML (Toms Opplagte,\
  \ Minimale Spr\xE5k) data ved bruk av Elixir. Programmerere bruker det til \xE5\
  \ h\xE5ndtere\u2026"
lastmod: '2024-03-13T22:44:40.466698-06:00'
model: gpt-4-0125-preview
summary: "\xC5 jobbe med TOML betyr \xE5 analysere og generere TOML (Toms Opplagte,\
  \ Minimale Spr\xE5k) data ved bruk av Elixir."
title: Jobbe med TOML
weight: 39
---

## Hva og hvorfor?
Å jobbe med TOML betyr å analysere og generere TOML (Toms Opplagte, Minimale Språk) data ved bruk av Elixir. Programmerere bruker det til å håndtere konfigurasjonsfiler fordi TOML er lesbart, enkelt å analysere, og kartlegger godt til en hash datastruktur.

## Hvordan:
Først, legg til en TOML-tolker i dine mix-avhengigheter. Dette eksemplet bruker `toml-elixir`:

```elixir
def deps do
  [
    {:toml_elixir, "~> 2.0"}
  ]
end
```

Les en TOML-fil:

```elixir
{:ok, toml_data} = File.read("config.toml")
{:ok, parsed_data} = TomlElixir.parse(toml_data)
```

For å konvertere Elixir-data til TOML:

```elixir
data = %{title: "TOML Example", owner: %{name: "Tom Preston-Werner"}}
toml_streng = TomlElixir.encode(data)
```

Eksempel på utdata:

```elixir
"title = \"TOML Example\"\n\n[owner]\nname = \"Tom Preston-Werner\"\n"
```

## Dypdykk
TOML ble skapt av Tom Preston-Werner, medgrunnlegger av GitHub, for bruk i konfigurasjonsfiler. Det er designet for å være mer rett frem enn XML og mer kortfattet enn YAML, samtidig som det opprettholder konsistens.

Alternativer inkluderer JSON, YAML, og INI-filer, hver med sine kompromisser i menneskelig lesbarhet og datastrukturkompatibilitet. TOML utmerker seg i å klart representere tabellære data og nestede grupperinger av data.

I Elixir, avhenger håndtering av TOML på dekoding og koding av biblioteker, som transformerer TOML-strenger til Elixir-maps og omvendt. Parsing fungerer ved å matche TOMLs syntaksregler og konvertere dem til Elixirs datatyper. Koding gjør det motsatte ved å kartlegge Elixirs datatyper tilbake til gyldig TOML-syntaks.

## Se også
- TOML-språket: https://toml.io/en/
- `toml-elixir` GitHub-repositorium: https://github.com/bitwalker/toml-elixir
- Hex-pakkedetaljer for `toml-elixir`: https://hex.pm/packages/toml_elixir
