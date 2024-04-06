---
date: 2024-01-20 17:30:47.492545-07:00
description: "How to: \xC5 beregne datoer i Fish er enkelt n\xE5, men det har ikke\
  \ alltid v\xE6rt slik. Fish har utviklet seg til \xE5 bli mer brukervennlig over\
  \ tid. Alternativer\u2026"
lastmod: '2024-04-05T22:50:55.254683-06:00'
model: gpt-4-1106-preview
summary: "\xC5 beregne datoer i Fish er enkelt n\xE5, men det har ikke alltid v\xE6\
  rt slik."
title: Beregning av en dato i fremtiden eller fortiden
weight: 26
---

## How to:
```Fish Shell
# Legger til 10 dager til den nåværende datoen
set future_date (date -d "+10 days" +"%Y-%m-%d")
echo $future_date

# Trekker fra 5 dager fra den nåværende datoen
set past_date (date -d "-5 days" +"%Y-%m-%d")
echo $past_date
```
Eksempel på utdata:
```
2030-04-14 # Hvis dagens dato var 2030-04-04
2030-03-30 # Hvis dagens dato var 2030-04-04
```

## Deep Dive
Å beregne datoer i Fish er enkelt nå, men det har ikke alltid vært slik. Fish har utviklet seg til å bli mer brukervennlig over tid. Alternativer som Bash bruker `date` på en lignende måte til Fish, men syntaksen kan variere. Med `date`-kommandoen representerer plusstegnet en fremtidig tid og minustegnet en tid i fortiden. Implementering av tidsberegning i Fish krever forståelse for Linux' `date`-kommando og riktig formatstreng.

## See Also
- Fish dokumentasjon på dato kommandoer: https://fishshell.com/docs/current/commands.html#date
- `man date` på Linux for å se alle muligheter med `date`-kommandoen.
- Stack Overflow for spørsmål om tidsberegning: https://stackoverflow.com/questions/tagged/date-calculation
