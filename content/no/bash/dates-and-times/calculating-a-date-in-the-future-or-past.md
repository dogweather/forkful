---
date: 2024-01-20 17:28:30.144085-07:00
description: "Hvordan gj\xF8r man det: Dato-beregning i Bash bruker kommandoen `date`,\
  \ som er standard p\xE5 de fleste UNIX-systemer. Tidligere l\xF8ste programmerere\u2026"
lastmod: '2024-04-05T22:50:54.990517-06:00'
model: gpt-4-1106-preview
summary: "Dato-beregning i Bash bruker kommandoen `date`, som er standard p\xE5 de\
  \ fleste UNIX-systemer."
title: Beregning av en dato i fremtiden eller fortiden
weight: 26
---

## Hvordan gjør man det:
```Bash
# Kalkuler en dato 10 dager frem i tid
date -d "10 days" '+%Y-%m-%d'

# Eksempel på output
2023-04-28

# Finn en dato 5 dager tidligere
date -d "5 days ago" '+%Y-%m-%d'

# Eksempel på output
2023-04-13
```

## Dypdykk
Dato-beregning i Bash bruker kommandoen `date`, som er standard på de fleste UNIX-systemer. Tidligere løste programmerere tidsproblemer med kompliserte algoritmer før interne datobiblioteker og kommandoer ble vanlige. Alternativer til `date` inkluderer andre programmeringsspråk som Python med `datetime`-modulen, eller bruk av online API-er for dato-manipulasjon.

Når det gjelder implementeringsdetaljer, tillater `date`-kommandoen flere format- og input-alternativer for å håndtere forskjellige scenarier. Det respekterer også tidssone-innstillinger og håndterer skuddår. Dette gjør at programmerere kan beregne datoer riktig uten å bekymre seg for de underliggende kompleksitetene.

## Se også
- [GNU Coreutils Manual](https://www.gnu.org/software/coreutils/manual/coreutils.html) for detaljert informasjon om `date`.
- [Advanced Bash-Scripting Guide](https://tldp.org/LDP/abs/html/) for generelle Bash-skriptingsteknikker og -tips.
- [Bash Date Command](https://ss64.com/bash/date.html) for en oversikt over format og opsjoner for `date`-kommandoen.
