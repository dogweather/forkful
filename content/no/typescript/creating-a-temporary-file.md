---
title:    "TypeScript: Opprette en midlertidig fil"
keywords: ["TypeScript"]
---

{{< edit_this_page >}}

## Hvorfor

Å opprette midlertidige filer kan være nyttig for å håndtere midlertidig data eller å lagre midlertidige resultater av en funksjon. Dette kan være spesielt nyttig i TypeScript-programmering for å maksimere effektivitet og sikkerhet.

## Hvordan gjøre det

For å opprette en midlertidig fil i TypeScript trenger man å importere fs-modulen, som tillater tilgang til filsystemet. Deretter kan man bruke metoden `mktempSync()` for å opprette en unik midlertidig fil.

```TypeScript
import * as fs from 'fs'; 

const tempFile = fs.mktempSync('tempFile-XXXXXX.txt');

console.log(tempFile); 
// Output: "tempFile-UHq964.txt"
```

Man kan også spesifisere en målsti for den midlertidige filen ved å legge til en andre argument i `mktempSync()`, som vist i eksemplet nedenfor:

```TypeScript
import * as fs from 'fs'; 

const tempFile = fs.mktempSync('tempFile-XXXXXX.txt', '/temp/');

console.log(tempFile); 
// Output: "/temp/tempFile-uG48hu.txt"
```

Man kan også bruke funksjoner som `readFileSync()` og `writeFileSync()` til å lese og skrive til den midlertidige filen.

```TypeScript
import * as fs from 'fs'; 

const data = 'Dette er et eksempel på innhold som skal skrives til den midlertidige filen';

fs.writeFileSync(tempFile, data);
const tempFileContent = fs.readFileSync(tempFile, 'utf-8');

console.log(tempFileContent); 
// Output: "Dette er et eksempel på innhold som skal skrives til den midlertidige filen"
```

## Dykk dypere

Når man bruker midlertidige filer, er det viktig å huske på å slette filen når man er ferdig med å bruke den. Dette kan gjøres ved å bruke metoden `unlinkSync()`, som sletter en fil fra filsystemet.

```TypeScript
import * as fs from 'fs'; 

fs.unlinkSync(tempFile);
```

Man kan også bruke en unik filbane-generator for å opprette en midlertidig filbane ved hjelp av `tmp`-modulen.

```TypeScript
import * as tmp from 'tmp'; 

const tempPath = tmp.dirSync().name;

console.log(tempPath); 
// Output: "/var/folders/h9/ty1bqshd7qbgs6k/__tmp0/"
```

## Se også

* [Node.js fs Modulen](https://nodejs.org/api/fs.html)
* [TypeScript Offisiell Nettside](https://www.typescriptlang.org/)
* [tmp Modulen](https://www.npmjs.com/package/tmp)