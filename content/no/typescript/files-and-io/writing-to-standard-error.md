---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:34:39.513912-07:00
description: "Hvordan: TypeScript, som er et superset av JavaScript, stoler p\xE5\
  \ den underliggende JS-runtime-milj\xF8et (som Node.js) for \xE5 skrive til stderr.\
  \ Slik kan du\u2026"
lastmod: '2024-03-13T22:44:40.548467-06:00'
model: gpt-4-0125-preview
summary: "TypeScript, som er et superset av JavaScript, stoler p\xE5 den underliggende\
  \ JS-runtime-milj\xF8et (som Node.js) for \xE5 skrive til stderr."
title: Skriving til standardfeil
weight: 25
---

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
