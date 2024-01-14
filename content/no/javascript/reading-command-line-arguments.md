---
title:    "Javascript: Lesing av kommandolinje-argumenter"
keywords: ["Javascript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/javascript/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

# Hvorfor?

Å lese kommandolinjeargumenter kan være en nyttig ferdighet for utviklere å ha i verktøykassen sin. Det kan gjøre det mulig å gjøre programmene dine mer fleksible ved å tillate brukerne å angi spesifikke parametere når de kjører programmet, som kan påvirke resultatene eller funksjonaliteten til programmet. Det kan også hjelpe med å feilsøke og teste programmet ditt med forskjellige datasett uten å måtte endre selve koden.

# Hvordan gjør du det?

Å lese kommandolinjeargumenter i Javascript er enkelt og kan gjøres ved hjelp av et innebygd objekt som heter "process". Dette objektet har en egenskap kalt "argv" som inneholder alle de gitte argumentene når programmet kjører.

For å lese argumentene, kan du bruke "process.argv[i]" der "i" er plasseringen til argumentet du vil lese. Den første argumenten (indeks 0) er alltid navnet på programmet ditt, mens de etterfølgende argumentene vil være de som ble angitt av brukeren.

Her er et enkelt eksempel på å lese to kommandolinjeargumenter og skrive dem ut til konsollen:

```Javascript
console.log(process.argv[0]); // output: "node"
console.log(process.argv[1]); // output: "programnavn.js"
console.log(process.argv[2]); // output: "arg1"
console.log(process.argv[3]); // output: "arg2"
```

I dette eksempelet vil "arg1" og "arg2" være argumentene som brukeren angav når de kjørte programmet.

# Dypdykk

Process.argv-objektet inneholder også andre nyttige egenskaper, som "argv0" som inneholder navnet på programmet ditt, "execPath" som inneholder stien til kjørebanen for node.exe, og "env" som inneholder alle miljøvariabler som er tilgjengelige.

I tillegg er det også muligheter for å bruke et tredjepartsmodul som "yargs" for å håndtere kommandolinjeargumenter mer elegant og enkelt. Denne modulen tilbyr funksjoner for å definere aksepterte argumenter og håndtere standardverdier, og til og med generere hjelpetekster for programmet ditt.

# Se også

- Dokumentasjon for "process" objektet: https://nodejs.org/docs/latest-v10.x/api/process.html
- Yargs modul: https://www.npmjs.com/package/yargs