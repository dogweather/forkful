---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:33:31.971778-07:00
description: "Das Schreiben auf den Standardfehler (stderr) in JavaScript bedeutet,\
  \ Fehlermeldungen oder jegliche kritische Informationen an einen spezifischen,\u2026"
lastmod: 2024-02-19 22:05:13.220284
model: gpt-4-0125-preview
summary: "Das Schreiben auf den Standardfehler (stderr) in JavaScript bedeutet, Fehlermeldungen\
  \ oder jegliche kritische Informationen an einen spezifischen,\u2026"
title: Schreiben auf Standardfehler
---

{{< edit_this_page >}}

## Was & Warum?
Das Schreiben auf den Standardfehler (stderr) in JavaScript bedeutet, Fehlermeldungen oder jegliche kritische Informationen an einen spezifischen, separaten Stream zu leiten, was besonders in Unix-ähnlichen Umgebungen für Protokollierungs- und Debuggingzwecke nützlich ist. Programmierer tun dies, um die normale Programmausgabe von Fehlermeldungen zu unterscheiden, was eine saubere Verwaltung der Ausgabe und eine einfachere Fehlerüberwachung ermöglicht.

## Wie:
In Node.js kann das Schreiben auf stderr mit der Methode `console.error()` oder durch direktes Schreiben auf `process.stderr` erreicht werden. Hier sind Beispiele, die beide Ansätze demonstrieren:

```javascript
// Verwenden von console.error()
console.error('Dies ist eine Fehlermeldung.');

// Direktes Schreiben auf process.stderr
process.stderr.write('Dies ist eine andere Fehlermeldung.\n');
```

Die Muster-Ausgaben für beide Methoden erscheinen im stderr-Stream, ohne sich mit stdout zu vermischen:
```
Dies ist eine Fehlermeldung.
Dies ist eine andere Fehlermeldung.
```

Für ausgefeiltere oder anwendungsspezifische Protokollierungen verwenden viele JavaScript-Programmierer Drittanbieter-Bibliotheken wie `winston` oder `bunyan`. Hier ist ein kurzes Beispiel unter Verwendung von `winston`:

Zuerst installiere `winston` via npm:
```shell
npm install winston
```

Konfiguriere dann `winston`, um Fehler auf stderr zu protokollieren:
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

// Protokollieren einer Fehlermeldung
logger.error('Fehler durch winston protokolliert.');
```

Diese Einrichtung stellt sicher, dass wenn du einen Fehler mit `winston` protokollierst, er auf stderr geleitet wird, was hilft, eine klare Trennung zwischen Standard- und Fehlerausgaben zu erhalten.
