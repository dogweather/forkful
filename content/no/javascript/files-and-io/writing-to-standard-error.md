---
title:                "Skriving til standardfeil"
aliases:
- no/javascript/writing-to-standard-error.md
date:                  2024-02-03T19:33:40.262426-07:00
model:                 gpt-4-0125-preview
simple_title:         "Skriving til standardfeil"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/javascript/writing-to-standard-error.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Å skrive til standardfeil (stderr) i JavaScript handler om å dirigere feilmeldinger eller kritisk informasjon til en spesifikk, separat strøm, noe som er spesielt nyttig i Unix-lignende miljøer for logging og feilsøking. Programmerere gjør dette for å skille normal programutdata fra feilmeldinger, noe som tillater enklere håndtering av utdata og lettere overvåking av feil.

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
