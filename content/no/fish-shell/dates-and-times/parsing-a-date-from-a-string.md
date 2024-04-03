---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:14:12.506058-07:00
description: "Hvordan: I Fish Shell har du ikke innebygde kommandoer spesielt designet\
  \ for \xE5 analysere datoer fra strenger. I stedet er du avhengig av eksterne verkt\xF8\
  y\u2026"
lastmod: '2024-03-13T22:44:41.237108-06:00'
model: gpt-4-0125-preview
summary: "I Fish Shell har du ikke innebygde kommandoer spesielt designet for \xE5\
  \ analysere datoer fra strenger."
title: Analysering av en dato fra en streng
weight: 30
---

## Hvordan:
I Fish Shell har du ikke innebygde kommandoer spesielt designet for å analysere datoer fra strenger. I stedet er du avhengig av eksterne verktøy som `date` (tilgjengelig på Linux og macOS) eller benytter populære tredjepartsverktøy som `GNU date` for mer kompleks analyse. Her er hvordan du kan nærme deg det:

**Bruke `date` med Fish:**

For å analysere en datostreng i formatet "ÅÅÅÅ-MM-DD", kan du bruke `date`-kommandoen med `-d` (eller `--date` for GNU date) alternativet etterfulgt av strengen. `+`-alternativet brukes til å formatere output.

```fish
set date_str "2023-04-01"
date -d $date_str +"%A, %d %B %Y"
# Utdata: Lørdag, 01 april 2023
```

For macOS (som krever et annet format for `-j` og `-f` flaggene):

```fish
set date_str "2023-04-01"
date -j -f "%Y-%m-%d" $date_str +"%A, %d %B %Y"
# Utdata: Lørdag, 01 april 2023
```

**Bruke GNU `date` for kompleks analyse:** 

GNU `date` er mer fleksibel med strengformater. Den kan automatisk oppdage mange vanlige datostrengformater uten å eksplisitt spesifisere inngangsformatet:

```fish
set complex_date_str "April 1, 2023 14:00"
date -d "$complex_date_str" '+%Y-%m-%d %H:%M:%S'
# Utdata: 2023-04-01 14:00:00
```

Derimot, når du jobber med datostrenger som kanskje ikke automatisk blir gjenkjent, eller når nøyaktig kontroll over inngangsformatet er nødvendig, er spesifisering av inngangsformatet med GNU `date` ikke direkte støttet. I slike tilfeller, vurder forhåndsbehandling av strengen eller bruk av et annet verktøy designet for mer komplekse datoparseringsrutiner.
