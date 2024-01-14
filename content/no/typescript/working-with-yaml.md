---
title:                "TypeScript: Å jobbe med yaml"
simple_title:         "Å jobbe med yaml"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/typescript/working-with-yaml.md"
---

{{< edit_this_page >}}

## Hvorfor
YAML er et kryssplattform dataformat som kan brukes til å strukturere og lagre data på en leservennlig måte. Det er spesielt nyttig for å konfigurere applikasjoner og skript.

## Slik gjør du det
For å begynne å jobbe med YAML i TypeScript, må du først installere yamljs biblioteket ved hjelp av npm-kommandoen:
```
npm install yamljs
```
Deretter kan du importere biblioteket og bruke `load` -funksjonen til å lese inn YAML-data fra en fil:
```TypeScript
import * as yaml from 'yamljs';
const data = yaml.load('config.yaml');
```
Du kan nå bruke `data` variabelen til å få tilgang til de strukturerte dataene.

For å skrive YAML-data til en fil, kan du bruke `dump` -funksjonen og angi ønsket filnavn:
```TypeScript
yaml.dump(data, "ny_config.yaml");
```
Dette vil skrive ut de strukturerte dataene fra `data` variabelen til en ny YAML-fil.

## Dypdykk
YAML støtter flere datatyper, inkludert strenger, tall, lister og objekter. Det er også mulig å bruke kommentarer og få tilgang til nøkkelen og verdien på en mer fleksibel måte ved hjelp av YAML-merker. Du kan utforske disse funksjonene mer i dybden i YAML-dokumentasjonen.

## Se også
- [YAML offisiell dokumentasjon](https://yaml.org/)
- [yamljs npm-modul](https://www.npmjs.com/package/yamljs)
- [TypeScript offisiell dokumentasjon](https://www.typescriptlang.org/)