---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:33:40.262426-07:00
description: "Hvordan: I Node.js kan skriving til stderr utf\xF8res ved \xE5 bruke\
  \ `console.error()`-metoden eller ved \xE5 skrive direkte til `process.stderr`.\
  \ Her er eksempler\u2026"
lastmod: '2024-03-13T22:44:41.199582-06:00'
model: gpt-4-0125-preview
summary: "I Node.js kan skriving til stderr utf\xF8res ved \xE5 bruke `console.error()`-metoden\
  \ eller ved \xE5 skrive direkte til `process.stderr`."
title: Skriving til standardfeil
weight: 25
---

## Hvordan:
I Node.js kan skriving til stderr utføres ved å bruke `console.error()`-metoden eller ved å skrive direkte til `process.stderr`. Her er eksempler som demonstrerer begge tilnærminger:

```javascript
// Bruker console.error()
console.error('Dette er en feilmelding.');

// Skriver direkte til process.stderr
process.stderr.write('Dette er en annen feilmelding.\n');
```

Eksempelutdata for begge metodene ville dukke opp i stderr-strømmen, ikke blandet med stdout:
```
Dette er en feilmelding.
Dette er en annen feilmelding.
```

For mer sofistikert eller applikasjonsspesifikk logging bruker mange JavaScript-programmerere tredjepartsbibliotek som `winston` eller `bunyan`. Her er et raskt eksempel som bruker `winston`:

Først, installer `winston` via npm:
```shell
npm install winston
```

Deretter, konfigurer `winston` for å logge feil til stderr:
```javascript
const winston = require('winston');

const logger = winston.createLogger({
  levels: winston.config.syslog.levels,
  transports: [
    new winston.transports.Console({
      stderrLevels: ['error']
    })
  ]
});

// Logger en feilmelding
logger.error('Feil logget gjennom winston.');
```

Denne oppsettet sikrer at når du logger en feil ved hjelp av `winston`, blir den dirigert til stderr, noe som hjelper med å opprettholde en klar separasjon mellom standard og feilutdata.
