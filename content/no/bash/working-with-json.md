---
title:                "Arbeid med JSON"
date:                  2024-01-19
html_title:           "Arduino: Arbeid med JSON"
simple_title:         "Arbeid med JSON"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/bash/working-with-json.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Jobbing med JSON handler om å håndtere data formatert som JavaScript Object Notation, vanlig for konfigurasjon og datautveksling på nettet. Utviklere gjør dette for å enkelt overføre data mellom servere og webapplikasjoner.

## Slik gjør du:
Manipulering av JSON i Bash krever vanligvis `jq` – en lettvektig kommandolinje JSON-prosessor. Først, installér `jq`:

```Bash
sudo apt-get install jq
```

La oss anta at vi har en JSON-fil, `eksempel.json`:

```JSON
{
  "brukere": [
    {"navn": "Kari", "alder": 34},
    {"navn": "Ola", "alder": 28}
  ]
}
```

For å hente ut alle navnene:

```Bash
cat eksempel.json | jq '.brukere[].navn'
```

Resultat:

```Bash
"Kari"
"Ola"
```

Oppdater en brukers alder:

```Bash
jq '.brukere[] | select(.navn=="Kari").alder = 35' eksempel.json
```

## Dybdeinformasjon:
`jq` ble introdusert i 2012 for å gjøre JSON-behandling enklere i kommandolinjen. Alternativer inkluderer `jshon` eller programmeringsspråkspesifikk kode (som Python's `json`-modul). Ved implementasjon må man ofte balansere mellom funksjonalitet og ytelse – `jq` er effektivt for små til medium store datasett, men store JSON-data kan kreve mer kraftfulle verktøy eller kode.

## Se også:
- `jq` manual: https://stedolan.github.io/jq/manual/
- Bash JSON tutorials: https://shapeshed.com/jq-json/
- Sammenligning av JSON-prosessorer: https://www.arp242.net/json-shootout.html
