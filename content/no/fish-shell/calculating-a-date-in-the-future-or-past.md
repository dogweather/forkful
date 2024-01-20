---
title:                "Beregning av en dato i fremtiden eller fortiden"
html_title:           "Fish Shell: Beregning av en dato i fremtiden eller fortiden"
simple_title:         "Beregning av en dato i fremtiden eller fortiden"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/fish-shell/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Å beregne en dato i fremtiden eller fortiden handler om å forflytte seg fra en spesifikk dato med en gitt varighet. Programmere gjør det for å håndtere tidssensitive oppgaver, som rollovers, påminnelser, eller tidslinje beregninger.

## Hvordan:
Her er en enkel mate å beregne fremtidige eller tidligere datoer i Fish Shell:
```Fish Shell
set -l date (date -u "+%Y-%m-%dT%H:%M:%S")
set -l future_date (date -u -d "$date + 7 day" "+%Y-%m-%dT%H:%M:%S")
set -l past_date (date -u -d "$date - 7 day" "+%Y-%m-%dT%H:%M:%S")
echo "Nåværende Dato: $date"
echo "Fremtidig Dato: $future_date"
echo "Tidligere Dato: $past_date"
```
Utgang fra ovennevnte kode vil være noe som:
```Fish Shell
Nåværende Dato: 2022-05-12T14:30:15
Fremtidig Dato: 2022-05-19T14:30:15
Tidligere Dato: 2022-05-05T14:30:15
```
## Dyp Dykk:
1. Historisk Kontekst: Mange tidlige programmeringsspråk manglet innebygd dato- og tidsstøtte, så utviklere måtte implementere egne beregninger for tid og dato. I dag har de fleste programmeringsspråk innebygd støtte for dato- og tidsmanipulasjoner, og Fish Shell er intet unntak.

2. Alternativer: Selv om Fish Shell har innebygd funksjonalitet for å beregne en dato i fremtiden eller fortiden, er det også andre alternativer som kan være hensiktsmessige, avhengig av situasjonen. Du kan bruke andre Unix-baserte kommandolinjeverktøy som `awk`, `perl`, `python`, osv. eller du kan bruke tredjepartsbibliotek hvis du bruker et programmeringsspråk som Python, JavaScript eller Ruby.

3. Implementasjon Detaljer: Ovennevnte Fish Shell kodebenytter Unix' "date"-kommando for å beregne fremtidige og tidligere datoer. Først, vi initialiserer en variabel "date" med den nåværende datoen i ISO 8601-format. Deretter lager vi "future_date" og "past_date" variabler ved å legge til eller trekke fra 7 dager fra "date". 

## Se Også:
Her er noen nyttige lenker relatert til dette emnet:
1. Fish Shell Dokumentasjon: [https://fishshell.com/docs/current/index.html](https://fishshell.com/docs/current/index.html)
3. WWikipedia's ISO 8601 artikkel: [https://en.wikipedia.org/wiki/ISO_8601](https://en.wikipedia.org/wiki/ISO_8601)