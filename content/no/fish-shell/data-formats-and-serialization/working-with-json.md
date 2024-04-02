---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:22:49.505996-07:00
description: "\xC5 jobbe med JSON i Fish Shell inneb\xE6rer parsing og generering\
  \ av JSON-data, en vanlig oppgave for konfigurering av applikasjoner, API-interaksjon,\
  \ og\u2026"
lastmod: '2024-03-13T22:44:41.248868-06:00'
model: gpt-4-0125-preview
summary: "\xC5 jobbe med JSON i Fish Shell inneb\xE6rer parsing og generering av JSON-data,\
  \ en vanlig oppgave for konfigurering av applikasjoner, API-interaksjon, og\u2026"
title: Arbeider med JSON
weight: 38
---

## Hva & Hvorfor?

Å jobbe med JSON i Fish Shell innebærer parsing og generering av JSON-data, en vanlig oppgave for konfigurering av applikasjoner, API-interaksjon, og effektivisering av kommandolinjeflyter. Gitt JSONs allestedsnærvær i web- og applikasjonsutvikling, kan mestring av manipulasjonen direkte i shellen betydelig forbedre automatisering og effektivitet i databehandling for programmerere.

## Hvordan:

Fish Shell har i seg selv ikke innebygde verktøy for å parse og generere JSON. Imidlertid integrerer den sømløst med tredjepartsverktøy som `jq` for JSON-behandling. `jq` er en kraftig og allsidig kommandolinje JSON-prosessor som lar deg skive, filtrere, kartlegge og transformere strukturerte data med et enkelt og uttrykksfullt språk.

### Parse JSON med jq
For å parse en JSON-fil og ekstrahere data ved hjelp av `jq`:

```fish
# Anta at du har en JSON-fil ved navn 'data.json' med innhold: {"name":"Fish Shell","version":"3.4.0"}
cat data.json | jq '.name'
# Eksempel på utdata
"Fish Shell"
```

### Generere JSON med jq
Lage JSON-innhold fra shell-variabler eller utdata:

```fish
# Opprett JSON-objekt fra variabler
set name "Fish Shell"
set version "3.4.0"
jq -n --arg name "$name" --arg version "$version" '{name: $name, version: $version}'
# Eksempel på utdata
{
  "name": "Fish Shell",
  "version": "3.4.0"
}
```

### Filtrere JSON-samlinger
Anta at vi har en JSON-array med objekter i en fil ved navn `versions.json`:
```json
[
  {"version": "3.1.2", "stable": true},
  {"version": "3.2.0", "stable": false},
  {"version": "3.4.0", "stable": true}
]
```
For å filtrere denne arrayen for kun stabile versjoner:

```fish
cat versions.json | jq '.[] | select(.stable == true) | .version'
# Eksempel på utdata
"3.1.2"
"3.4.0"
```

Eksemplene som er gitt demonstrerer kraften av å integrere `jq` med Fish Shell for JSON-operasjoner. Å utnytte slike verktøy beriker shell-opplevelsen, noe som gjør den til et formidabelt miljø for håndtering av moderne dataformater.
