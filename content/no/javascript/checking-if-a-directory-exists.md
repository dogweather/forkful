---
title:                "Javascript: Sjekke om en mappe eksisterer"
simple_title:         "Sjekke om en mappe eksisterer"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/javascript/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Hvorfor

Å sjekke om en mappe eksisterer er en viktig del av programmering for å sikre at koden vår fungerer som den skal. Dette er spesielt nyttig når vi arbeider med filbehandling og trenger å vite om en mappe allerede finnes eller om vi må opprette den.

## Slik gjør du det

For å sjekke om en mappe eksisterer i JavaScript, kan vi bruke fs modulen som er innebygd i Node.js. Vi kan først importere denne modulen ved å skrive følgende kode øverst i JavaScript-filen vår:

```Javascript
const fs = require('fs');
```

Deretter kan vi bruke fs.existsSync() funksjonen for å sjekke om en mappe eksisterer eller ikke. Denne funksjonen tar inn som argument en sti til mappen vi vil sjekke. Her er et eksempel på hvordan vi kan bruke denne funksjonen:

```Javascript
if(fs.existsSync('./mappenavn')){
  console.log("Mappen eksisterer!");
} else{
  console.log("Mappen eksisterer ikke. Oppretter en ny mappe...");
  fs.mkdirSync('./mappenavn'); // Oppretter en ny mappe hvis den ikke eksisterer
}
```

I dette eksemplet sjekker vi om mappen "mappenavn" eksisterer i den nåværende arbeidsmappen vår. Hvis mappen eksisterer, vil vi få en melding som bekrefter dette. Hvis ikke, vil vi få en melding om at mappen ikke eksisterer og deretter vil en ny mappe bli opprettet.

## Dypere dykk

Det er verdt å merke seg at fs.existsSync() funksjonen returnerer en boolean (true eller false), avhengig av om mappen eksisterer eller ikke. Dette betyr at vi også kan bruke denne funksjonen i en if-setning, som vist i eksempelet ovenfor. Vi kan også bruke denne funksjonen til å sjekke om en fil eksisterer, ved å endre stien som argument til funksjonen.

Det er også verdt å nevne at fs.existsSync() funksjonen er synkron, dette betyr at den vil blokkere resten av koden vår mens sjekken pågår. Dette bør vurderes dersom du arbeider med store mengder filer og mapper.

## Se også

- [Node.js dokumentasjon om fs modulen](https://nodejs.org/api/fs.html)
- [Tutorialspoint guide om å sjekke om en mappen eksisterer i Node.js](https://www.tutorialspoint.com/nodejs/mkdirsync.htm)