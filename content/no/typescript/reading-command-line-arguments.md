---
title:                "Lesing av kommandolinje-argumenter"
html_title:           "TypeScript: Lesing av kommandolinje-argumenter"
simple_title:         "Lesing av kommandolinje-argumenter"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/typescript/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Lesing av argumenter fra kommandolinjen i TypeScript er en måte for programmerere å få tilgang til informasjon som sendes inn når programmet kjører. Dette kan være nyttig for å kunne tilpasse programmet til forskjellige situasjoner eller brukerpreferanser.

## Hvordan:
```TypeScript
// Enkelt eksempel på å lese et enkelt argument fra kommandolinjen
const argument = process.argv[2];
console.log(`Argumentet fra kommandolinjen er: ${argument}`);

// Eksempel på å lese flere argumenter og lagre de i et array
const argumenter = process.argv.slice(2);
console.log(`Alle argumenter fra kommandolinjen er: ${argumenter}`);
```

Eksempel output:
```
$ node readCommandLineArguments.ts --navn Bob --alder 30
Argumentet fra kommandolinjen er: --navn
Alle argumenter fra kommandolinjen er: --navn, Bob, --alder, 30
```

## Dypdykk:
Lesing av argumenter fra kommandolinjen er en vanlig praksis i de fleste programmeringsspråk. Dette ble introdusert for å gi programmerere mulighet til å gi input til et program når det kjøres, i tillegg til å gjøre det mer fleksibelt for å tilpasse programmet til forskjellige situasjoner.

En alternativ måte å lese argumenter fra kommandolinjen på er å bruke et tredjeparts bibliotek, som for eksempel "yargs". Dette kan gi en mer strukturert og enkel måte å håndtere argumentene på.

Når du leser argumenter fra kommandolinjen i TypeScript, vil argumentene være tilgjengelige i prosessens "argv" array. Det første argumentet i arrayet vil være selve kommandoet som ble brukt for å kjøre programmet, mens de påfølgende argumentene vil være eventuelle input gitt av brukeren.

## Se også:
- [Dokumentasjon for prosessmodulen i Node.js](https://nodejs.org/api/process.html)
- [Yargs dokumentasjon](https://github.com/yargs/yargs)