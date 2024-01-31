---
title:                "Skrive til standardfeil"
date:                  2024-01-19
html_title:           "Arduino: Skrive til standardfeil"
simple_title:         "Skrive til standardfeil"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/javascript/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## What & Why?
Skriving til standard error (stderr) lar deg rapportere feil og logger uten å forstyrre vanlig output. Programmerere bruker det for å skille vanlige data fra feilmeldinger og for å lettere feilsøke.

## How to:
```javascript
// Skriv til stderr i Node.js
process.stderr.write('Dette er en feilmelding.\n');

// Eller med console.error
console.error('Noe gikk galt!');

// Sample output når du kjører koden:
// Dette er en feilmelding.
// Noe gikk galt!
```

## Deep Dive:
Historisk sett har kommandolinjeverktøy brukt standard output (stdout) og stderr for å henholdsvis skrive ut resultater og feil. I JavaScript kan du skrive til stderr i Node.js, ikke direkte i nettlesere. Implementasjonsdetaljer inkluderer `process`-objektet i Node.js hvor `stderr` er en skrivbar stream. Som et alternativ kan logger som Bunyan eller Winston lede loggdata til forskjellige utløp, inkludert stderr.

## See Also:
- [Node.js `process` documentation](https://nodejs.org/api/process.html)
- [Console API reference on MDN](https://developer.mozilla.org/en-US/docs/Web/API/console/error)
- [Stream API in Node.js](https://nodejs.org/api/stream.html)
- [Winston logger](https://github.com/winstonjs/winston)
- [Bunyan logger](https://github.com/trentm/node-bunyan)
