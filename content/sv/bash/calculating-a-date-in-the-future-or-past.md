---
title:                "Beräkna ett datum i framtiden eller förflutet"
date:                  2024-01-20T17:28:29.724641-07:00
model:                 gpt-4-1106-preview
html_title:           "Bash: Beräkna ett datum i framtiden eller förflutet"
simple_title:         "Beräkna ett datum i framtiden eller förflutet"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/bash/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att räkna ut ett framtida eller förflutet datum betyder att man lägger till eller drar ifrån dagar till ett givet datum. Programmerare gör det för att hantera tidsintervall, som att beräkna utgångsdatum eller schemalägga händelser.

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
