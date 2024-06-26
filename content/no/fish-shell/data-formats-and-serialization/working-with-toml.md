---
date: 2024-01-26 04:21:33.734363-07:00
description: "Hvordan: For \xE5 lese og manipulere TOML i Fish kan du bruke et verkt\xF8\
  y som `yj`, som kan konvertere TOML til JSON. Slik gj\xF8r du det."
lastmod: '2024-03-13T22:44:41.250853-06:00'
model: gpt-4-0125-preview
summary: "For \xE5 lese og manipulere TOML i Fish kan du bruke et verkt\xF8y som `yj`,\
  \ som kan konvertere TOML til JSON."
title: Jobbe med TOML
weight: 39
---

## Hvordan:
For å lese og manipulere TOML i Fish kan du bruke et verktøy som `yj`, som kan konvertere TOML til JSON. Slik gjør du det:

```fish
# Installer yj via Fisher
fisher install jorgebucaran/yj

# Konverter TOML til JSON
echo 'title = "TOML Example"' | yj -tj

# Eksempelutskrift
{"title":"TOML Example"}
```

For å skrive TOML reverserer du prosessen:

```fish
# Konverter JSON til TOML
echo '{"title":"JSON Example"}' | yj -jt

# Eksempelutskrift
title = "JSON Example"
```

For tunge løft, vurder et dedikert TOML CLI-verktøy som `toml-cli`.

```fish
# Installer toml-cli
pip install toml-cli

# Sett en verdi i TOML-filen
toml set pyproject.toml tool.poetry.version "1.1.4"

# Hent en verdi fra TOML-filen
set version (toml get pyproject.toml tool.poetry.version)
echo $version
```

## Dypdykk
TOML (Tom's Obvious, Minimal Language), introdusert av Tom Preston-Werner i 2013, er i slekt med INI men med en definert spesifikasjon og datahierarki. JSON og YAML er de viktigste alternativene, men de har sine avveininger: JSON er ikke så menneskevennlig, mens YAML er mer komplekst. TOMLs design utfolder seg i scenarier der config-filer ofte vedlikeholdes for hånd, balanserer enkelhet og uttrykksfullhet. Når det kommer til implementering, er TOML-tolkere tilgjengelige for de fleste programmeringsspråk, inkludert TomlBombadil for Fish som kan passe rett inn i skriptene dine.

## Se også
- TOML Offisiell spesifikasjon: https://toml.io
- `yj`, et verktøy for å konvertere mellom TOML, JSON, YAML og XML: https://github.com/jorgebucaran/yj
- `toml-cli`, et kommandolinjeverktøy for TOML: https://github.com/sdispater/toml-cli
