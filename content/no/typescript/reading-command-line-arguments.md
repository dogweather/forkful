---
title:                "TypeScript: Lese kommandolinje-argumenter"
simple_title:         "Lese kommandolinje-argumenter"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/typescript/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Hvorfor

Mange programmer har behov for å kunne ta imot og behandle informasjon gitt gjennom kommandolinjen. Dette kan være for å få spesifikk informasjon fra brukeren, som filnavn eller søkeord, eller for å kjøre forskjellige funksjoner avhengig av hvilke argumenter som er gitt. Enten du er en nybegynner eller en erfaren utvikler, er det viktig å forstå hvordan du kan lese kommandolinje-argumenter i TypeScript for å lage mer fleksible og brukervennlige programmer.

## Hvordan

Det er flere måter å lese kommandolinje-argumenter i TypeScript på, men en enkel og vanlig måte er ved hjelp av Node.js `process` objektet. Dette objektet gir tilgang til kommandolinjen i Node.js applikasjoner og lar deg hente ut informasjon gitt av brukeren.

For å lese argumenter gitt gjennom kommandolinjen, må du først importere `process` objektet:
```TypeScript
import * as process from 'process';
```

Deretter kan du bruke `process.argv` til å få en liste over alle argumentene som er gitt. Dette er en array, hvor det første elementet er banen til Node.js, og de neste elementene er argumentene.
```TypeScript
const args = process.argv;
```

For å få tak i spesifikke argumenter, kan du bruke indeksering. For eksempel, hvis du ønsker å få tak i det tredje argumentet, kan du bruke `args[2]`.

La oss se på et eksempel hvor vi ønsker å få tak i en fil som brukeren har gitt som argument, og deretter skrive ut innholdet i denne filen:
```TypeScript
import * as process from 'process';
import * as fs from 'fs';

const args = process.argv;
const fileName = args[2];

fs.readFile(fileName, 'utf-8', (error, data) => {
  if (error) {
    console.log('Kunne ikke lese filen. Feilmelding:', error.message);
    return;
  }

  console.log('Innholdet i filen er:', data);
});
```

I dette eksempelet bruker vi også `fs` modulen for å lese filen som brukeren har gitt som argument.

## Deep Dive

I noen tilfeller kan det også være nyttig å kunne gi egendefinerte argumenter når du kjører din TypeScript applikasjon. Dette kan gjøres ved å bruke `--` etterfulgt av navnet på argumentet og verdien.
```bash
ts-node app.ts --navn John --alder 25
```

I koden kan du lese disse argumentene ved hjelp av `process.argv.slice(2)`, og deretter konvertere dem til objekter ved å splitte dem og bruke `reduce()` funksjonen:
```TypeScript
const customArgs = process.argv.slice(2).reduce((acc, curr) => {
  const [arg, value] = curr.split('=');
  acc[arg] = value;
  return acc;
}, {});

console.log(customArgs.navn); // John
console.log(customArgs.alder); // 25
```

Dette er spesielt nyttig for å gjøre applikasjonen din mer fleksibel og avhengig av hvilke argumenter brukeren gir, kan du kjøre forskjellige funksjoner eller endre oppførselen til programmet ditt.

## Se Også

- [Node.js Process | Node.js API Documentation](https://nodejs.org/api/process.html)
- [Top 5 Ways to Parse Command Line Arguments in Node.js](https://stackabuse.com/top-5-ways-to-parse-command-line-arguments-in-node-js/)
- [Understanding Command Line Arguments in Node.js](https://codeburst.io/understanding-command-line-arguments-in-node-js-d832715c5158?gi=d416d3321add)