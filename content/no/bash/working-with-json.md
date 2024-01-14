---
title:                "Bash: Å jobbe med json"
simple_title:         "Å jobbe med json"
programming_language: "Bash"
category:             "Bash"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/bash/working-with-json.md"
---

{{< edit_this_page >}}

## Hvorfor

I dagens digitale verden er det svært vanlig å jobbe med data i form av JSON. JSON, eller JavaScript Object Notation, er et format for å lagre og utveksle data. Det brukes mye i webapplikasjoner og APIer, og som en Bash programmerer kan det være nyttig å ha kunnskap om hvordan man håndterer JSON data.

## Hvordan

Det første skrittet for å jobbe med JSON i Bash er å skaffe seg en JSON parser. Det finnes flere alternativer tilgjengelig, inkludert jq og jsawk. I dette eksempelet vil vi bruke jq for å demonstrere hvordan man kan jobbe med JSON i Bash.

Først må du lagre JSON data i en variabel ved å bruke en kommando som wget eller curl for å hente data fra en URL. Deretter kan du bruke jq til å håndtere dataene. Her er et eksempel på hvordan du kan liste ut navnene på de 10 øverste landene fra en liste av land og deres befolkning:

```Bash
# Hent JSON data fra en URL og lagre det i en variabel
raw_data=$(curl -s "https://restcountries.eu/rest/v2/all")

# Bruk jq til å hente ut navnene på de 10 øverste landene
names=$(echo $raw_data | jq -r '.[:10] .name')

# Skriv ut resultatet
echo $names
```

Eksempeloutput:

```
Afghanistan
Albania
Algeria
Andorra
Angola
Antigua and Barbuda
Argentina
Armenia
Australia
Austria
```

Det er også mulig å bruke jq til å filtrere ut data basert på visse kriterier. For eksempel kan vi filtrere ut land som har et areal større enn 500000 og sortere dem etter befolkningstall:

```Bash
# Bruk jq til å hente ut land med areal større enn 500000 og sortere basert på befolkning
raw_data | jq '.[] | select(.area > 500000) | {name: .name, population: .population} | sort_by(.population)'
```

Eksempeloutput:

```
[{
  "name": "Australia",
  "population": 24117360
}, {
  "name": "Norway",
  "population": 5367580
}, {
  "name": "Namibia",
  "population": 2113077
}, {
  "name": "Botswana",
  "population": 2338851
}, {
  "name": "Senegal",
  "population": 16209125
}, {
  "name": "Azerbaijan",
  "population": 9923914
}, {
  "name": "Cameroon",
  "population": 22709892
}, {
  "name": "Myanmar",
  "population": 51419420
}, {
  "name": "Zambia",
  "population": 15933883
}, {
  "name": "Papua New Guinea",
  "population": 8606316
}]
```

## Dypdykk

En dypere forståelse av JSON vil være nyttig når man jobber med komplekse datasamlinger. JSON kan inneholde flere nivåer av data, og man kan bruke nestede objekter og lister. I tillegg kan man bruke forskjellige typer datatyper, som strenger, tall, boolske verdier og null-verdier.

Det er også viktig å være oppmerksom på at JSON data er case-sensitive, så det er viktig å bruke riktig syntax når du jobber med dataene. Det kan være nyttig å lese på dokumentasjonen til APIet eller databasen du henter data fra for å sikre at du behandler dataene på riktig måte.

## Se også

- [jq: Kommandolinjetjenesten for JSON](https://stedolan.github.io/jq/)
- [jsawk: Et JSON verktøy for awk](https://github.com/micha/jsawk)
- [Bash JSON parsing og håndtering av kompliserte datastrukturer](https://eloquentcoder.com/bash-json-parsing-bash-awk-json-structure/)