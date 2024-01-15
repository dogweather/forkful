---
title:                "Oppretting av en midlertidig fil"
html_title:           "TypeScript: Oppretting av en midlertidig fil"
simple_title:         "Oppretting av en midlertidig fil"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/typescript/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Hvorfor

Å lage midlertidige filer kan være nyttig når du jobber med programmering. Disse filene kan inneholde midlertidig informasjon som du kanskje ikke ønsker å lagre permanent, men som er nødvendig for at koden din skal fungere riktig.

## Slik gjør du det

Midlertidige filer kan enkelt opprettes ved å bruke TypeScript-koden «fs.writeFileSync». Her er et eksempel på hvordan du kan opprette en midlertidig fil og skrive innhold i den:

```TypeScript
import * as fs from 'fs';

// Opprett en midlertidig fil
fs.writeFileSync('temp.txt', 'Dette er en midlertidig fil.');

// Les innholdet i filen
let data = fs.readFileSync('temp.txt', 'utf8');
console.log(data); // Vil skrive ut "Dette er en midlertidig fil."
```

I dette eksempelet bruker vi «fs.writeFileSync» til å opprette en fil med navnet «temp.txt» og skrive innholdet «Dette er en midlertidig fil.» i den. Deretter bruker vi «fs.readFileSync» til å lese innholdet fra filen og skrive det ut i konsollen.

## En dypere dykk

Å opprette midlertidige filer er spesielt nyttig når du jobber med store datamengder eller når du trenger å håndtere midlertidig informasjon på en effektiv måte. Ved å bruke «fs.writeFileSync» kan du spesifisere filens navn og innhold, samt hvilken type encoding du ønsker å bruke.

I tillegg til å opprette midlertidige filer, kan du også bruke TypeScript til å slette dem når du er ferdig med å bruke dem. Dette kan gjøres ved å bruke «fs.unlinkSync»-funksjonen, som lar deg slette en fil ved å oppgi filnavnet som en parameter.

## Se også

[Offisiell TypeScript-dokumentasjon om fs-modulen](https://www.typescriptlang.org/docs/handbook/fs.html)

[Mer om å lage, lese og skrive filer med TypeScript](https://www.digitalocean.com/community/tutorials/typescript-type-inference-file-system)