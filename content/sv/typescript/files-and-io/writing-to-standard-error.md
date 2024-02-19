---
aliases:
- /sv/typescript/writing-to-standard-error/
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:34:44.711928-07:00
description: "I TypeScript \xE4r skrivning till standardfel (stderr) en process f\xF6\
  r att skicka felmeddelanden eller loggar direkt till milj\xF6ns felutdatastr\xF6\
  m (t.ex.\u2026"
lastmod: 2024-02-18 23:08:51.559310
model: gpt-4-0125-preview
summary: "I TypeScript \xE4r skrivning till standardfel (stderr) en process f\xF6\
  r att skicka felmeddelanden eller loggar direkt till milj\xF6ns felutdatastr\xF6\
  m (t.ex.\u2026"
title: Skriva till standardfel
---

{{< edit_this_page >}}

## Vad & Varför?
I TypeScript är skrivning till standardfel (stderr) en process för att skicka felmeddelanden eller loggar direkt till miljöns felutdataström (t.ex. konsolen i node.js eller en webbläsare). Detta är avgörande för att diagnostisera problem utan att störa standardutdata (stdout) som vanligtvis används för programdata, för att säkerställa att felhantering och loggning hanteras effektivt och sammanhängande.

## Hur man gör:
TypeScript, som är ett övergripande till JavaScript, förlitar sig på den underliggande JS-körmiljön (som Node.js) för att skriva till stderr. Så här kan du göra det direkt:

```typescript
console.error("Detta är ett felmeddelande.");
```

Exempelutdata till stderr:
```
Detta är ett felmeddelande.
```

I en Node.js-miljö kan du också använda metoden `process.stderr.write()` för mer lågnivåskrivning:

```typescript
process.stderr.write("Felmeddelande på låg nivå.\n");
```

Exempelutdata till stderr:
```
Felmeddelande på låg nivå.
```

För mer strukturerad felskrivning kan man använda populära tredjepartsbibliotek som `winston` eller `pino`. Så här loggar du fel med hjälp av `winston`:

Först, installera `winston`:

```bash
npm install winston
```

Använd sedan det i din TypeScript-fil:

```typescript
import * as winston from 'winston';

const logger = winston.createLogger({
  levels: winston.config.syslog.levels,
  transports: [
    new winston.transports.Console(),
    new winston.transports.File({ filename: 'error.log', level: 'error' })
  ],
});

logger.error('Fel loggat med winston.');
```

Detta kommer att skriva felet till både konsolen och en fil med namnet `error.log`. Kom ihåg, när du skriver till filer är det viktigt att hantera filrättigheter och övergång för att förhindra problem relaterade till diskanvändning.
