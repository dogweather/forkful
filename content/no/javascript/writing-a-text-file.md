---
title:                "Skriving av en tekstfil"
date:                  2024-01-19
simple_title:         "Skriving av en tekstfil"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/javascript/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Writing a text file in JavaScript means creating and saving data to a file, typically on your computer. Programmers do this for data persistence, configuration, or sharing between systems and users.

## Slik gjør du:
For å skrive til en tekstfil i Node.js, bruker vi `fs`-modulen. Her er et raskt eksempel:

```javascript
const fs = require('fs');

let data = 'Dette er en test tekstfil.';

fs.writeFile('eksempel.txt', data, (err) => {
  if(err) throw err;
  console.log('Filen har blitt lagret!');
});
```

Kjører du koden, opprettes 'eksempel.txt' med teksten vi ga den. Om filen allerede eksisterer, blir den overskrevet.

## Deep Dive
Opprinnelig, i tidligere versjoner av JavaScript, var det ikke mulig å skrive filer direkte fra nettleser-konteksten av sikkerhetsårsaker. I Node.js kan dette gjøres med innebygde moduler som `fs`. Alternativer for filskriving i en nettleser inkluderer bruk av lokale lagrings-APIer som `localStorage` eller `IndexedDB`. Det å skrive til en fil involverer typisk å bruke metoder som `writeFile` for å lagre tekst eller `writeFileSync` for en synkron versjon som blokkerer tråden til filen blir skrevet.

## Se også
- [`fs`-modulens dokumentasjon hos Node.js](https://nodejs.org/api/fs.html)
- MDN's guide til [local storage](https://developer.mozilla.org/en-US/docs/Web/API/Window/localStorage)
- MDN's guide til [IndexedDB](https://developer.mozilla.org/en-US/docs/Web/API/IndexedDB_API)
