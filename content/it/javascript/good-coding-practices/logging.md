---
date: 2024-01-26 01:07:18.275264-07:00
description: "Il logging, in poche parole, \xE8 come tenere un diario per la tua applicazione:\
  \ registra eventi, errori e altre azioni significative che avvengono mentre il\u2026"
lastmod: '2024-03-13T22:44:43.818928-06:00'
model: gpt-4-1106-preview
summary: "Il logging, in poche parole, \xE8 come tenere un diario per la tua applicazione:\
  \ registra eventi, errori e altre azioni significative che avvengono mentre il\u2026"
title: "Registrazione delle Attivit\xE0 (Logging)"
---

{{< edit_this_page >}}

## Cosa e Perché?
Il logging, in poche parole, è come tenere un diario per la tua applicazione: registra eventi, errori e altre azioni significative che avvengono mentre il software è in esecuzione. I programmatori lo fanno non solo per capire cosa succede sotto il cofano in tempo reale, ma anche per avere una registrazione storica che è fondamentale per il debugging, l'audit e l'ottimizzazione delle prestazioni.

## Come fare:
JavaScript offre di base un modo semplice per registrare messaggi nella console:

```javascript
console.log('Questo verrà registrato nella console');

// Output:
// Questo verrà registrato nella console
```

Ma le applicazioni del mondo reale richiedono più che stampare messaggi nella console. Si possono introdurre librerie come Winston o Pino per gestire efficacemente i log:

```javascript
// Usando Winston per logging avanzato
const winston = require('winston');

const logger = winston.createLogger({
  level: 'info',
  format: winston.format.json(),
  transports: [
    new winston.transports.File({ filename: 'combined.log' })
  ],
});

logger.info('Ciao, questo è un evento di logging con Winston');
// Questo log viene scritto in 'combined.log' in formato JSON
```

Esempio di output di `combined.log`:

```json
{"message":"Ciao, questo è un evento di logging con Winston","level":"info"}
```

## Approfondimento
Il logging è essenziale fin dai primi giorni del computing; gli operatori di sistema esaminavano i log per comprendere le prestazioni del sistema e diagnosticare i problemi. Oggi, nello sviluppo moderno, siamo passati da semplici file di log a sistemi di gestione dei log strutturati e ricercabili.

Le alternative al logging nella console o su file in JavaScript includono l'utilizzo di servizi di logging basati sul cloud come Loggly, Datadog o ELK Stack (Elasticsearch, Logstash, Kibana) che possono aggregare log da molteplici fonti, offrendo strumenti di visualizzazione e analisi avanzate.

Quando si implementa il logging, considerare i seguenti aspetti:
- **Livello di Dettaglio**: Includendo debug, info, warning, error e critical.
- **Prestazioni**: Un eccesso di logging può impattare sulle prestazioni dell'applicazione.
- **Sicurezza**: Prestare attenzione a non registrare informazioni sensibili.
- **Formato**: I log strutturati (come JSON) rendono più facile la ricerca e l'analisi dei log.
- **Politiche di Conservazione**: I vecchi log devono essere archiviati o eliminati per risparmiare spazio.

Una strategia di logging pratica definisce cosa registrare, dove registrarla e per quanto tempo conservare i dati, bilanciando informazioni dettagliate rispetto a considerazioni di prestazione e privacy.

## Vedi anche
Consulta queste risorse per un approfondimento:
- [Winston GitHub Repository](https://github.com/winstonjs/winston): per un utilizzo approfondito e trasporti personalizzati.
- [Pino - Logger per Node.js molto leggero](https://github.com/pinojs/pino): una soluzione di logging leggera.
- [MDN Web Docs: Console](https://developer.mozilla.org/en-US/docs/Web/API/Console): per informazioni di base sul logging basato su browser.
- [Elastic ELK Stack](https://www.elastic.co/what-is/elk-stack): un potente trio per la gestione dei log.
- [12 Factor App Logging](https://12factor.net/logs): le migliori pratiche nel logging delle applicazioni.
