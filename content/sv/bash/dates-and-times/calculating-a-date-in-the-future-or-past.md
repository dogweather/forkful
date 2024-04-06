---
date: 2024-01-20 17:28:29.724641-07:00
description: "Hur g\xF6r man: Tillbaka p\xE5 70-talet kunde man bara dr\xF6mma om\
  \ enkla kommandon f\xF6r att manipulera datum. Unix `date` kommandot lades till\
  \ p\xE5 tidigt 80-tal och\u2026"
lastmod: '2024-04-05T22:50:52.400379-06:00'
model: gpt-4-1106-preview
summary: "Tillbaka p\xE5 70-talet kunde man bara dr\xF6mma om enkla kommandon f\xF6\
  r att manipulera datum."
title: "Ber\xE4kna ett datum i framtiden eller f\xF6rflutet"
weight: 26
---

## Hur gör man:
```Bash
# Räkna ut datumet för 10 dagar sedan
datum=$(date -d "10 days ago" '+%Y-%m-%d')
echo $datum

# Räkna ut datumet om 10 dagar
datum=$(date -d "10 days" '+%Y-%m-%d')
echo $datum
```
Exempelutdata:
```
2023-03-25  # datumet för 10 dagar sedan baserat på dagens datum
2023-04-14  # datumet om 10 dagar från dagens datum
```

## Djupdykning
Tillbaka på 70-talet kunde man bara drömma om enkla kommandon för att manipulera datum. Unix `date` kommandot lades till på tidigt 80-tal och har sedan dess genomgått många förbättringar. Alternativ inkluderar användning av andra programmeringsspråk eller verktyg som `GNU date` för mer avancerade funktioner, exempelvis tidszoner. Implementationsmässigt använder `date` tidsstämplar och kalenderberäkningar för att hantera datumoperationer.

## Se även
- GNU Coreutils Manual: https://www.gnu.org/software/coreutils/manual/coreutils.html#date-invocation
- Bash Scripting Guide: https://www.tldp.org/LDP/abs/html/dates.html
