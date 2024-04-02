---
date: 2024-01-26 04:19:10.421191-07:00
description: "TOML, som st\xE5r for Toms \xE5penbare, minimale spr\xE5k, er et format\
  \ for dataserialisering. Programmerere digger det for dets enkelhet og lesbarhet;\
  \ det er\u2026"
lastmod: '2024-03-13T22:44:40.997952-06:00'
model: gpt-4-0125-preview
summary: "TOML, som st\xE5r for Toms \xE5penbare, minimale spr\xE5k, er et format\
  \ for dataserialisering. Programmerere digger det for dets enkelhet og lesbarhet;\
  \ det er\u2026"
title: Jobbe med TOML
weight: 39
---

## Hva & Hvorfor?
TOML, som står for Toms åpenbare, minimale språk, er et format for dataserialisering. Programmerere digger det for dets enkelhet og lesbarhet; det er prikke for konfigurasjonsfiler, lignende vibes som YAML, men mindre klønete enn JSON for et menneske.

## Hvordan:
Først, installer `toml-cli` for å leke med TOML i Bash. Praktisk for å lese eller redigere TOML-filer på sparket.

```Bash
# Installer toml-cli, vår lille hjelper for TOML-oppgaver
pip install toml-cli

# Tenk deg at du har en TOML-fil, 'config.toml'
echo -e 'title = "TOML Demo"\n\n[owner]\nname = "Tom"\ndob = 1979-05-27T07:32:00Z' > config.toml

# Les en verdi
toml get config.toml owner.name
# Utgang: Tom

# Sett en verdi
toml set config.toml 'owner.dob' '2000-01-01T00:00:00Z'
# Pro tips: Bruk anførselstegn for nøkler med punktum eller funky tegn!
```

## Dykk dypere
Født ut av mislikningen av JSONs hinder for mennesker, dukket TOML opp rundt 2013. Tom Preston-Werner, GitHub sin medgründer, ønsket noe superlesbart. YAML og INI var alternativer, men TOML er som det beste av begge.

Bam, du har nestede data og arrays, minus YAMLs fotfeller og JSONs krøllete parenteser. TOML er nå et go-to for konfig i Rusts Cargo, noe som taler til dens vekst i utviklerverdenen. Det drives av en spesifikasjon, som holder ting stramt og veldefinert. Du vil nabbe parser i nesten hvilket som helst språk, noe som gjør det bredt adopterbart.

## Se også
- Offisielle TOML GitHub Repo: https://github.com/toml-lang/toml
- toml-cli på PyPI: https://pypi.org/project/toml-cli/
- Sammenligning av data-serieliseringsformater: https://en.wikipedia.org/wiki/Comparison_of_data-serialization_formats
