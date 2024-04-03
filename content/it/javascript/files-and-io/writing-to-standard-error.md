---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:33:37.398493-07:00
description: "Come: In Node.js, scrivere su stderr pu\xF2 essere realizzato utilizzando\
  \ il metodo `console.error()` o scrivendo direttamente su `process.stderr`. Ecco\u2026"
lastmod: '2024-03-13T22:44:43.829743-06:00'
model: gpt-4-0125-preview
summary: "In Node.js, scrivere su stderr pu\xF2 essere realizzato utilizzando il metodo\
  \ `console.error()` o scrivendo direttamente su `process.stderr`."
title: Scrivere sull'errore standard
weight: 25
---

## Come:
In Node.js, scrivere su stderr può essere realizzato utilizzando il metodo `console.error()` o scrivendo direttamente su `process.stderr`. Ecco degli esempi che dimostrano entrambi gli approcci:

```javascript
// Usando console.error()
console.error('Questo è un messaggio di errore.');

// Scrivendo direttamente su process.stderr
process.stderr.write('Questo è un altro messaggio di errore.\n');
```

L'output di esempio per entrambi i metodi apparirebbe nel flusso stderr, senza mischiarsi con stdout:
```
Questo è un messaggio di errore.
Questo è un altro messaggio di errore.
```

Per una registrazione più sofisticata o specifica dell'applicazione, molti programmatori JavaScript utilizzano librerie di terze parti come `winston` o `bunyan`. Ecco un rapido esempio usando `winston`:

Prima, installa `winston` tramite npm:
```shell
npm install winston
```

Poi, configura `winston` per registrare gli errori su stderr:
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

// Registrando un messaggio di errore
logger.error('Errore registrato attraverso winston.');
```

Questa configurazione assicura che quando registri un errore usando `winston`, questo viene indirizzato su stderr, aiutando a mantenere una chiara separazione tra output standard e errori.
