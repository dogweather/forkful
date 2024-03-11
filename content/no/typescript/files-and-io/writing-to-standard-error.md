---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:34:39.513912-07:00
description: "I TypeScript er skriving til standardfeil (stderr) en prosess for \xE5\
  \ sende feilmeldinger eller logger direkte til milj\xF8ets feilutdatastr\xF8m (for\
  \ eksempel\u2026"
lastmod: '2024-03-11T00:14:14.080075-06:00'
model: gpt-4-0125-preview
summary: "I TypeScript er skriving til standardfeil (stderr) en prosess for \xE5 sende\
  \ feilmeldinger eller logger direkte til milj\xF8ets feilutdatastr\xF8m (for eksempel\u2026"
title: Skriving til standardfeil
---

{{< edit_this_page >}}

## Hva & hvorfor?
I TypeScript er skriving til standardfeil (stderr) en prosess for å sende feilmeldinger eller logger direkte til miljøets feilutdatastrøm (for eksempel konsollen i node.js eller en nettleser). Dette er essensielt for diagnostisering av problemer uten å forstyrre standardutdata (stdout), som vanligvis brukes for programdata, og sikrer at feilhåndtering og logging håndteres effektivt og sammenhengende.

## Hvordan:
TypeScript, som er et superset av JavaScript, stoler på den underliggende JS-runtime-miljøet (som Node.js) for å skrive til stderr. Slik kan du gjøre det direkte:

```typescript
console.error("Dette er en feilmelding.");
```

Eksempelutdata til stderr:
```
Dette er en feilmelding.
```

I et Node.js-miljø kan du også bruke `process.stderr.write()`-metoden for mer lavnivåskriving:

```typescript
process.stderr.write("Lavnivå feilmelding.\n");
```

Eksempelutdata til stderr:
```
Lavnivå feilmelding.
```

For mer strukturert feillogging, kan du bruke populære tredjepartsbiblioteker som `winston` eller `pino`. Slik logger du feil ved hjelp av `winston`:

Først, installer `winston`:

```bash
npm install winston
```

Deretter bruker du det i din TypeScript-fil:

```typescript
import * as winston from 'winston';

const logger = winston.createLogger({
  nivåer: winston.config.syslog.levels,
  transports: [
    new winston.transports.Console(),
    new winston.transports.File({ filename: 'error.log', level: 'error' })
  ],
});

logger.error('Feil logget ved bruk av winston.');
```

Dette vil skrive feilen til både konsollen og en fil med navn `error.log`. Husk, når du skriver til filer, er det viktig å håndtere filtillatelser og rollover for å forhindre problemer knyttet til diskplassbruk.
