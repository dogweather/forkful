---
title:    "Javascript: Sjekke om en mappe eksisterer"
keywords: ["Javascript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/javascript/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Hvorfor

Å sjekke om en mappe eksisterer er en viktig del av programmering, spesielt når man jobber med filbehandling. Dette gjør det mulig å unngå feil og sikre at programmet kjører uten problemer.

## Hvordan

Det finnes flere måter å sjekke om en mappe eksisterer i Javascript, men den vanligste og anbefalte metoden er å bruke FileSystem API. Dette gir tilgang til filsystemet på datamaskinen og gjør det mulig å utføre operasjoner som å sjekke om en mappe eksisterer.

```Javascript
const fs = require('fs');
const path = './min_mappe';

// Sjekker om mappen eksisterer
fs.existsSync(path) //Output: true hvis mappen finnes, false hvis den ikke finnes
```

Man kan også bruke promises og async/await for å gjøre koden mer lesbar og håndtere eventuelle feil. Her er et eksempel på hvordan man kan sjekke om en mappe eksisterer asynkront:

```Javascript
const fs = require('fs').promises;
const path = './min_mappe';

// Sjekker om mappen eksisterer
async function sjekkMappe() {
  try {
    const exists = await fs.access(path);
    return true; // mappen eksisterer
  } catch (error) {
    if (error.code === 'ENOENT') {
      return false; // mappen finnes ikke
    }
    console.error(error); // håndter andre feil
  }
}
```

## Deep Dive

Når man bruker FileSystem API for å sjekke om en mappe eksisterer, brukes metoden `existsSync` eller `access` avhengig av versjonen av NodeJS. Denne metoden tar inn to argumenter: en streng som representerer stien til mappen vi vil sjekke, og en callback-funksjon som kalles når operasjonen er ferdig.

Det første argumentet, stien til mappen, må være en relativ eller absolutt sti. Hvis det er en relativ sti, vil det bli konvertert til en absolutt sti basert på plasseringen til kjørefilen. Det andre argumentet er en funksjon som tar inn en eventuell feil og en boolean som sier om mappen eksisterer eller ikke.

## Se også

- [FileSystem API](https://nodejs.org/api/fs.html)
- [Node.js - Checking if a directory exists](https://www.geeksforgeeks.org/node-js-fs-existsync-method/)
- [How to check if a file or directory exists in Node.js](https://attacomsian.com/blog/nodejs-check-if-file-directory-exists)