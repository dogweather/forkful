---
title:                "Javascript: Lesing av kommandolinje-argumenter"
simple_title:         "Lesing av kommandolinje-argumenter"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/javascript/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Hvorfor

Lesing av kommandolinje-argumenter er en viktig del av Javascript-programmering, spesielt når man ønsker å gi brukere muligheten til å tilpasse og kontrollere programmets oppførsel ved hjelp av argumenter. Det er også nyttig når man ønsker å kjøre visse funksjoner eller skrive ut spesifikk informasjon uten å måtte endre selve koden hver gang.

## Hvordan

Det er enkelt å lese kommandolinje-argumenter ved hjelp av Javascripts `process.argv`-funksjon. Her er et enkelt eksempel for å illustrere det:

```Javascript
// script.js
console.log(process.argv);
```

I terminalen kan du skrive inn følgende kommando:

```
node script.js arg1 arg2
```

Dette vil gi output som dette:

```
[ 'node',
  '/path/to/script.js',
  'arg1',
  'arg2' ]
```

Som du kan se, er `process.argv` en array som inneholder kommando, filnavn og argumentene som ble gitt.

For å få tilgang til de spesifikke argumentene, kan du bruke array-indeksene. For eksempel, for å få tilgang til `arg1`, må du bruke `process.argv[2]` siden kommando og filnavn teller som indeks 0 og 1.

```Javascript
// script.js
console.log(`Argument 1: ${process.argv[2]}`);
console.log(`Argument 2: ${process.argv[3]}`);
```

Dette vil gi følgende output:

```
Argument 1: arg1
Argument 2: arg2
```

## Dypdykk

Det finnes flere måter å lese og behandle kommandolinje-argumenter på, som for eksempel å bruke en pakke som `yargs` eller `commander` for å gi mer fleksibilitet og funksjonalitet. Det er også mulig å bruke flagger og flaggverdier for å gi alternativer som kan endre programmets oppførsel.

Det er viktig å huske på at kommandolinje-argumenter kan være følsomme for trykkfeil og feil formatting, så det kan være lurt å ha noen form for validering på plass.

## Se også

- [Hvordan lage et enkelt kommandolinjeverktøy med Node.js](https://www.digitalocean.com/community/tutorials/how-to-build-a-command-line-application-with-node-js)
- [Yargs dokumentasjon](https://www.npmjs.com/package/yargs)
- [Commander dokumentasjon](https://www.npmjs.com/package/commander)