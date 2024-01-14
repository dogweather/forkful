---
title:                "Fish Shell: Arbeid med yaml"
simple_title:         "Arbeid med yaml"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/fish-shell/working-with-yaml.md"
---

{{< edit_this_page >}}

## Hvorfor

Hvis du er en programmerer som jobber med YAML-filer, er det sannsynligvis fordi du jobber med konfigurasjon eller datastrukturer i prosjektet ditt. YAML er et tekstbasert format som er lett å lese og skrive, og det er spesielt nyttig for å representere hierarkiske data. Ved å bruke Fish Shell kan du enkelt håndtere og manipulere YAML-filer for å gjøre arbeidet ditt mer effektivt.

## Hvordan

For å begynne å jobbe med YAML-filer i Fish Shell, trenger du å installere pakken "yq" ved hjelp av kommandoen `fisher install yq`. Deretter kan du bruke kommandoen `yq` etterfulgt av en handling (som å lese, skrive eller oppdatere) for å manipulere YAML-filer.

For å lese en YAML-fil og vise innholdet i terminalen, kan du bruke følgende kommando:

```Fish Shell
yq read filnavn.yaml
```

Du kan også bruke `yq` til å oppdatere en verdi i en YAML-fil, for eksempel:

```Fish Shell
yq write filnavn.yaml key.underkey ny_verdi
```

For å legge til en ny verdienes nøkkel og verdi i en YAML-fil, kan du bruke kommandoen:

```Fish Shell
yq write filnavn.yaml key.nykey ny_verdi
```

For å slette en nøkkel-verdi-par fra en YAML-fil, kan du bruke kommandoen:

```Fish Shell
yq delete filnavn.yaml key.underkey
```

## Dypdykk

Selv om det er enklest å jobbe med YAML i enkeltkommandoer som vist ovenfor, kan det også være nyttig å ha mer kontroll over manipulasjonen av YAML-data. Fish Shell har innebygde funksjoner som lar deg gjøre dette.

For eksempel kan `set` kommandoen brukes til å søke gjennom en yaml-fil og sette en verdi til en bestemt nøkkel. `get` kommandoen kan brukes til å hente en verdi fra en YAML-fil.

I tillegg har Fish Shell også en rekke nyttige og kraftige funksjoner som lar deg jobbe med YAML-data på mer komplekse måter. Dette inkluderer ting som å lage nye nøkler og verdier, sammenføyning av YAML-filer og mer.

## Se også

For mer informasjon om å jobbe med YAML i Fish Shell, kan du ta en titt på følgende ressurser:

- [Fish Shell dokumentasjon](https://fishshell.com/docs/current/index.html)
- [Fish Shell GitHub repository](https://github.com/fish-shell/fish-shell)
- [yq GitHub repository](https://github.com/mikefarah/yq)