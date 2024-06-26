---
date: 2024-01-26 01:08:40.173329-07:00
description: "Wie geht das: In TypeScript k\xF6nnen Sie einfaches Logging problemlos\
  \ mit Hilfe von Konsolenmethoden umsetzen oder erweitertes Logging mit Bibliotheken\
  \ wie\u2026"
lastmod: '2024-03-13T22:44:53.637472-06:00'
model: gpt-4-1106-preview
summary: "In TypeScript k\xF6nnen Sie einfaches Logging problemlos mit Hilfe von Konsolenmethoden\
  \ umsetzen oder erweitertes Logging mit Bibliotheken wie `winston` oder `pino` integrieren."
title: Protokollierung
weight: 17
---

## Wie geht das:
In TypeScript können Sie einfaches Logging problemlos mit Hilfe von Konsolenmethoden umsetzen oder erweitertes Logging mit Bibliotheken wie `winston` oder `pino` integrieren. Hier ein einfaches Beispiel mit `console.log` und ein fortgeschritteneres mit `winston`.

```TypeScript
// Einfaches Konsolen-Logging
console.log('Info: Anwendung wird gestartet...');
console.error('Fehler: Daten können nicht abgerufen werden.');

// Beispiel-Ausgabe
// Info: Anwendung wird gestartet...
// Fehler: Daten können nicht abgerufen werden.
```

Für eine robustere Protokollierung richten wir `winston` ein:

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

logger.info('Server gestartet!');
logger.warn('Warnung: Wenig Speicherplatz.');
logger.error('Verbindungsaufbau zur Datenbank fehlgeschlagen.');

// Beispiel-Ausgabe in combined.log
// 2023-01-20 14:42:07 info: Server gestartet!
// 2023-01-20 14:42:09 warn: Warnung: Wenig Speicherplatz.
// 2023-01-20 14:42:12 error: Verbindungsaufbau zur Datenbank fehlgeschlagen.
```

## Vertiefung:
Das Konzept des Loggings im Kontext der Informatik reicht zurück bis in die Anfangstage der Programmierung, wobei der Begriff selbst vom "Logbuch", einem nautischen Aufzeichnungssystem, abgeleitet ist. Historisch gesehen wurden Programmereignisse oft auf physischen Ausdrucken oder Terminalausgaben protokolliert, besonders während der Mainframe-Ära.

Bis heute haben Sie eine Fülle von Tools und Bibliotheken zur Verfügung, die verschiedene Logging-Anforderungen bedienen, von einfachen Textdateien bis hin zu komplexen Log-Management-Systemen. Alternativen zu `winston` sind unter anderem `pino`, das für hohe Leistung bekannt ist, und `Bunyan`, das auf JSON basiert. Bei der Arbeit mit Node.js bieten Logging-Bibliotheken oft Stream-Mechanismen, um Logs zu verschiedenen Zielen zu leiten, unterstützen die Logrotation und bieten anpassbare Formatter.

In der Implementierung enthalten Log-Nachrichten typischerweise einen Zeitstempel, ein Schweregrad-Level (wie info, warn, error) und die eigentliche Nachricht. Gute Praxis im Logging empfiehlt, die Log-Level angemessen zu kategorisieren, sensible Daten in Logs zu vermeiden und Leistungseinbußen in Anwendungen mit hohem Durchsatz zu bedenken.

## Siehe auch:
- [Winston - Ein Logger für so ziemlich alles](https://www.npmjs.com/package/winston)
- [Pino - Ein Node.js-Logger mit sehr geringer Systembelastung](https://www.npmjs.com/package/pino)
- [Best Practices für das Logging in Node.js](https://thisdavej.com/using-winston-a-versatile-logging-library-for-node-js/)
- [The 12 Factor App - Logs](https://12factor.net/logs)
