---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:34:38.838300-07:00
description: "Come fare: TypeScript, essendo un sovrainsieme di JavaScript, si affida\
  \ all'ambiente di runtime JS sottostante (come Node.js) per scrivere su stderr.\
  \ Ecco\u2026"
lastmod: '2024-03-13T22:44:43.192405-06:00'
model: gpt-4-0125-preview
summary: TypeScript, essendo un sovrainsieme di JavaScript, si affida all'ambiente
  di runtime JS sottostante (come Node.js) per scrivere su stderr.
title: Scrivere sull'errore standard
weight: 25
---

## Come fare:
TypeScript, essendo un sovrainsieme di JavaScript, si affida all'ambiente di runtime JS sottostante (come Node.js) per scrivere su stderr. Ecco come puoi farlo direttamente:

```typescript
console.error("Questo è un messaggio di errore.");
```

Esempio di output su stderr:
```
Questo è un messaggio di errore.
```

In un ambiente Node.js, puoi anche utilizzare il metodo `process.stderr.write()` per scrivere a un livello più basso:

```typescript
process.stderr.write("Messaggio di errore di basso livello.\n");
```

Esempio di output su stderr:
```
Messaggio di errore di basso livello.
```

Per una registrazione degli errori più strutturata, potresti usare librerie di terze parti popolari come `winston` o `pino`. Ecco come registrare gli errori utilizzando `winston`:

Prima, installa `winston`:

```bash
npm install winston
```

Quindi usalo nel tuo file TypeScript:

```typescript
import * as winston from 'winston';

const logger = winston.createLogger({
  levels: winston.config.syslog.levels,
  transports: [
    new winston.transports.Console(),
    new winston.transports.File({ filename: 'error.log', level: 'error' })
  ],
});

logger.error('Errore registrato usando winston.');
```

Questo scriverà l'errore sia sulla console che su un file denominato `error.log`. Ricorda, quando scrivi su file, è importante gestire i permessi dei file e il loro rollover per prevenire problemi relativi all'uso dello spazio su disco.
