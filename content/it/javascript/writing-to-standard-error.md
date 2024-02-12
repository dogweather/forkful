---
title:                "Scrivere sull'errore standard"
aliases:
- it/javascript/writing-to-standard-error.md
date:                  2024-02-03T19:33:37.398493-07:00
model:                 gpt-4-0125-preview
simple_title:         "Scrivere sull'errore standard"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/javascript/writing-to-standard-error.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cosa & Perché?
Scrivere su standard error (stderr) in JavaScript riguarda l'indirizzare messaggi di errore o qualsiasi informazione critica verso un flusso specifico e separato, il che è particolarmente utile negli ambienti simili a Unix per fini di registrazione e debugging. I programmatori fanno ciò per differenziare l'output normale del programma dai messaggi di errore, permettendo così una gestione più pulita dell'output e un monitoraggio degli errori più semplice.

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
