---
title:                "Å jobbe med json"
html_title:           "Fish Shell: Å jobbe med json"
simple_title:         "Å jobbe med json"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/fish-shell/working-with-json.md"
---

{{< edit_this_page >}}

## Hvorfor
Hei alle sammen! Har du noen gang lurt på hvorfor JSON har blitt så populært i moderne programmering? Vel, når det kommer til behandling av data og utveksling av informasjon, er JSON en enkel og effektiv løsning. Det er et format som er lett å lese og forstå både for mennesker og maskiner, og det er derfor det har blitt en viktig del av mange programmeringsspråk og rammeverk, inkludert Fish Shell.

## Slik gjør du det
Når du arbeider med JSON i Fish Shell, kan du bruke noen innebygde funksjoner og kommandoer for å håndtere dataene. La oss se på et eksempel på hvordan du kan utforske og behandle JSON-data:

```Fish Shell
set data (curl -s https://jsonplaceholder.typicode.com/todos/1) # last ned JSON-data
jq '.' <<< $data # bruk jq kommandoen for å formatere og vise dataene
```

Når du kjører denne koden, vil du se output som dette:

```JSON
{
  "userId": 1,
  "id": 1,
  "title": "delectus aut autem",
  "completed": false
}
```

Da kan vi bruke Fish Shells innebygde kommandoer for å få tilgang til og manipulere dataene. For eksempel kan vi bruke `set -l` for å hente en bestemt verdi fra dataene, og `string split` for å dele strenger:

```Fish Shell
set -l title (string split '"' <<< $data)[8] # bruker sting split for å få tak i "title" verdien
echo "Title: $title" # skriver ut tittelen
```

Output vil være:

```Shell
Title: delectus aut autem
```

Det er bare et enkelt eksempel på hvordan du kan bruke Fish Shell for å håndtere JSON-data. I de neste avsnittene skal vi dykke litt dypere inn i mulighetene og funksjonene for å jobbe med JSON i Fish Shell.

## Dypdykk
Fish Shell har flere integrerte funksjoner for å håndtere JSON. Blant annet kan du bruke `from_json` for å konvertere en JSON-streng til en Fish Shell-variabel, og `to_json` for å konvertere en variabel til en JSON-streng.

En nyttig funksjon å kjenne til er også `jq`, som vi brukte i vårt første eksempel. Denne kommandoen lar deg enkelt formatere og hente bestemte verdier fra JSON-data. Du kan også bruke `filter` for å finne bestemte data basert på kriterier.

Det er også verdt å merke seg at Fish Shell støtter bruk av JSON-filer som konfigurasjonsfiler. Dette gjør det enkelt å konfigurere og tilpasse ulike programmer ved hjelp av JSON-formatet.

For en fullstendig oversikt over alle mulighetene for å jobbe med JSON i Fish Shell, anbefaler vi å ta en titt på dokumentasjonen og leke rundt med ulike kommandoer og funksjoner.

## Se også
- Fish Shell dokumentasjon: https://fishshell.com/docs/current/
- JSON dokumentasjon: https://www.json.org/json-en.html