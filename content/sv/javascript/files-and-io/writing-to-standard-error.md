---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:33:39.107706-07:00
description: "Att skriva till standardfel (stderr) i JavaScript handlar om att dirigera\
  \ felmeddelanden eller kritisk information till en specifik, separat str\xF6m, vilket\u2026"
lastmod: '2024-02-25T18:49:36.624622-07:00'
model: gpt-4-0125-preview
summary: "Att skriva till standardfel (stderr) i JavaScript handlar om att dirigera\
  \ felmeddelanden eller kritisk information till en specifik, separat str\xF6m, vilket\u2026"
title: Skriva till standardfel
---

{{< edit_this_page >}}

## Vad & Varför?
Att skriva till standardfel (stderr) i JavaScript handlar om att dirigera felmeddelanden eller kritisk information till en specifik, separat ström, vilket är särskilt användbart i Unix-lika miljöer för loggning och felsökning. Programmerare gör detta för att skilja normal programutdata från felmeddelanden, vilket möjliggör enklare hantering av utdata och enklare övervakning av fel.

## Hur:
I Node.js kan skrivning till stderr utföras med metoden `console.error()` eller genom att skriva direkt till `process.stderr`. Här är exempel som demonstrerar båda tillvägagångssätten:

```javascript
// Använda console.error()
console.error('Det här är ett felmeddelande.');

// Skriva direkt till process.stderr
process.stderr.write('Det här är ett annat felmeddelande.\n');
```

Exempelutdata för båda metoderna skulle visas i stderr-strömmen, utan att blanda sig med stdout:
```
Det här är ett felmeddelande.
Det här är ett annat felmeddelande.
```

För mer sofistikerad eller applikationsspecifik loggning använder många JavaScript-programmerare tredjepartsbibliotek som `winston` eller `bunyan`. Här är ett snabbt exempel med `winston`:

Först, installera `winston` via npm:
```shell
npm install winston
```

Konfigurera sedan `winston` att logga fel till stderr:
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

// Loggar ett felmeddelande
logger.error('Fel loggat genom winston.');
```

Denna setup säkerställer att när du loggar ett fel med `winston`, dirigeras det till stderr, vilket hjälper till att upprätthålla en tydlig separation mellan standard- och felutdata.
