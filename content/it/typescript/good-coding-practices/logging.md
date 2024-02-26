---
date: 2024-01-26 01:09:07.847179-07:00
description: "Il logging \xE8 il processo di registrazione di eventi, errori e altre\
  \ informazioni significative durante l'esecuzione di un programma su un supporto\u2026"
lastmod: '2024-02-25T18:49:41.059426-07:00'
model: gpt-4-1106-preview
summary: "Il logging \xE8 il processo di registrazione di eventi, errori e altre informazioni\
  \ significative durante l'esecuzione di un programma su un supporto\u2026"
title: "Registrazione delle Attivit\xE0 (Logging)"
---

{{< edit_this_page >}}

## Cosa & Perché?

Il logging è il processo di registrazione di eventi, errori e altre informazioni significative durante l'esecuzione di un programma su un supporto esterno, spesso file o database. I programmatori utilizzano i log per monitorare il comportamento del software, risolvere problemi e tracciare le attività del sistema per analisi di sicurezza e performance.

## Come fare:

In TypeScript, è possibile implementare facilmente un logging di base utilizzando i metodi della console o integrare un logging più avanzato con librerie come `winston` o `pino`. Ecco un esempio di base che utilizza `console.log` e uno più avanzato con `winston`.

```TypeScript
// Logging di base della console
console.log('Info: Avvio dell\'applicazione...');
console.error('Errore: Impossibile recuperare i dati.');

// Output di esempio
// Info: Avvio dell'applicazione...
// Errore: Impossibile recuperare i dati.
```

Per un logging più robusto, configuriamo `winston`:

```TypeScript
import { createLogger, format, transports } from 'winston';

const logger = createLogger({
  level: 'info',
  format: format.combine(
    format.timestamp({ format: 'YYYY-MM-DD HH:mm:ss' }),
    format.printf(info => `${info.timestamp} ${info.level}: ${info.message}`)
  ),
  transports: [
    new transports.Console(),
    new transports.File({ filename: 'combined.log' })
  ]
});

logger.info('Server avviato!');
logger.warn('Attenzione: Spazio su disco basso.');
logger.error('Errore durante la connessione al database.');

// Output di esempio in combined.log
// 2023-01-20 14:42:07 info: Server avviato!
// 2023-01-20 14:42:09 warn: Attenzione: Spazio su disco basso.
// 2023-01-20 14:42:12 error: Errore durante la connessione al database.
```

## Approfondimento:

Il concetto di logging nel contesto dell'informatica risale ai primi giorni della programmazione, dove il termine stesso deriva dal "logbook", un sistema di registrazione utilizzato in ambito nautico. Storicamente, gli eventi dei programmi venivano spesso registrati su stampati fisici o output del terminale, specialmente durante l'era dei mainframe.

Oggi, abbiamo a disposizione una pletora di strumenti e librerie che soddisfano varie esigenze di logging, da semplici file di testo a complessi sistemi di gestione dei log. Alternative a `winston` includono `pino`, che vanta alte prestazioni, e `Bunyan`, che si basa su JSON. Quando si lavora con Node.js, le librerie di logging offrono spesso meccanismi di stream per indirizzare i log verso diverse destinazioni, supporto per la rotazione dei log e formattatori personalizzabili.

Dal punto di vista dell'implementazione, i messaggi di log contengono tipicamente un timestamp, un livello di gravità (come info, warn, error) e il messaggio effettivo. Una buona pratica di logging raccomanda di categorizzare correttamente i livelli di log, evitare dati sensibili nei log e considerare le implicazioni sulle performance in applicazioni ad alto traffico.

## Vedi Anche:

- [Winston - Un logger per quasi tutto](https://www.npmjs.com/package/winston)
- [Pino - Un logger Node.js molto leggero](https://www.npmjs.com/package/pino)
- [Best Practices per il Logging in Node.js](https://thisdavej.com/using-winston-a-versatile-logging-library-for-node-js/)
- [The 12 Factor App - Logs](https://12factor.net/logs)
