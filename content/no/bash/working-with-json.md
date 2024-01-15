---
title:                "Arbeide med json"
html_title:           "Bash: Arbeide med json"
simple_title:         "Arbeide med json"
programming_language: "Bash"
category:             "Bash"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/bash/working-with-json.md"
---

{{< edit_this_page >}}

## Hvorfor

Vi bruker JSON (JavaScript Object Notation) for å lagre, overføre og håndtere data i ulike programmeringsspråk. JSON er et lettvektsformat som er enkelt å lese og skrive for både mennesker og maskiner. Å jobbe med JSON i Bash kan være nyttig når du trenger å behandle data i skript eller automatisere oppgaver.

## Hvordan

Å jobbe med JSON i Bash er enkelt og krever bare noen få verktøy. Først må du sørge for at du har installert det populære verktøyet `jq` som lar deg manipulere og lese JSON-data. Du kan installere det ved å kjøre følgende kommando:

```Bash
sudo apt-get install jq
```
Nå kan du håndtere JSON-data ved hjelp av `jq` kommandoen. La oss si at du har en JSON-fil som inneholder en liste over brukere og deres alder. Du kan bruke `jq` for å filtrere ut bare brukerne over 25 år, ved å kjøre følgende kommando:

```Bash
jq '.[] | select(.age > 25)' users.json
```
Dette vil vise resultatet i en lesbar JSON-format. Du kan også bruke `jq` for å velge spesifikke felter fra JSON-data, eller for å legge til og endre data. Se dokumentasjonen for mer detaljert informasjon om hvordan du bruker `jq`.

## Dypdykk

Bash tilbyr også støtte for å konvertere JSON til bash-assosiative arrays (assosiativt tabell) ved hjelp av `declare` kommandoen. Dette kan være nyttig når du trenger å behandle data fra en annen kilde, for eksempel en nett-API, og bruke den til å sette variabler i Bash. 

Et eksempel på dette er å bruke [cURL](https://curl.haxx.se/), et annet populært verktøy i Bash, til å hente data fra en API som returnerer JSON-data. Du kan da bruke `jq` til å filtrere ut spesifikke felter fra svaret, og deretter bruke `declare` til å sette variabler med verdiene.

## Se også

- [jq dokumentasjon](https://stedolan.github.io/jq/manual/)
- [cURL homepage](https://curl.haxx.se/)