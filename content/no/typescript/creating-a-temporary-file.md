---
title:                "TypeScript: Å opprette en midlertidig fil"
simple_title:         "Å opprette en midlertidig fil"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/typescript/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Hvorfor
Det å opprette midlertidige filer kan være nyttig når man trenger å lagre data midlertidig under kjøring av et program. Dette kan være for å organisere og strukturere data, eller for å sikre at dataene ikke blir permanent lagret på systemet.

## Hvordan
For å opprette en midlertidig fil i TypeScript, kan man bruke Node.js' `fs` modul. Først må man importere modulen ved å legge til følgende linje øverst i koden:

```TypeScript
import * as fs from 'fs';
```

Deretter kan man bruke `fs`'s `tmpfile()` metode for å opprette en midlertidig fil og returnere dens bane:

```TypeScript
const tempFilePath: string = fs.tmpfile();
```

Man kan også angi en prefix eller suffiks for å gi filen et unikt navn ved å bruke `options` parametere:

```TypeScript
const tempFilePath: string = fs.tmpfile({
    prefix: 'temp_',
    suffix: '.txt'
});
```

Hvis man ønsker å skrive til den midlertidige filen, kan man bruke `writeFile()` metoden som følgende:

```TypeScript
fs.writeFile(tempFilePath, 'Dette er en midlertidig fil', (err) => {
    if (err) {
        throw err;
    }

    console.log('Data skrevet til midlertidig fil');
});
```

Når man er ferdig med å bruke den midlertidige filen, kan man slette den ved å bruke `unlink()` metoden:

```TypeScript
fs.unlink(tempFilePath, (err) => {
    if (err) {
        throw err;
    }

    console.log('Midlertidig fil slettet');
});
```

Et komplett eksempel på opprettelse og bruk av en midlertidig fil i TypeScript kan se slik ut:

```TypeScript
import * as fs from 'fs';

// Oppretter en midlertidig fil med suffix '.txt'
const tempFilePath: string = fs.tmpfile({ suffix: '.txt' });

// Skriver data til filen
fs.writeFile(tempFilePath, 'Dette er en midlertidig fil', (err) => {
    if (err) {
        throw err;
    }

    console.log('Data skrevet til midlertidig fil');
});

// Sletter filen når man er ferdig med å bruke den
fs.unlink(tempFilePath, (err) => {
    if (err) {
        throw err;
    }

    console.log('Midlertidig fil slettet');
});
```

Output av koden vil være:

```bash
Data skrevet til midlertidig fil
Midlertidig fil slettet
```

## Dypdykk
Når man oppretter en midlertidig fil, vil operativsystemet automatisk velge en plassering for filen basert på systemets innstillinger. Man kan også angi en spesifikk plassering ved å bruke `tmpdir` parameter i `tmpfile()` metoden og gi den en gyldig bane.

I tillegg kan man også angi andre options som f.eks. `encoding` og `mode` for å spesifisere filens encoding og tillatelser.

Det er også viktig å merke seg at den midlertidige filen vil bli automatisk slettet når programmet termineres eller når man kaller `unlink()` metoden.

## Se også
- [Node.js fs-modulen dokumentasjon](https://nodejs.org/api/fs.html)
- [Temporary folder for any OS in Node.js](https://stackoverflow.com/questions/31039653/temporary-folder-for-any-os-in-nodejs)