---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:34:35.149980-07:00
description: "Wie zu: TypeScript, als eine Obermenge von JavaScript, st\xFCtzt sich\
  \ auf die zugrunde liegende JS-Laufzeitumgebung (wie Node.js) f\xFCr das Schreiben\
  \ auf\u2026"
lastmod: '2024-03-13T22:44:53.646828-06:00'
model: gpt-4-0125-preview
summary: "TypeScript, als eine Obermenge von JavaScript, st\xFCtzt sich auf die zugrunde\
  \ liegende JS-Laufzeitumgebung (wie Node.js) f\xFCr das Schreiben auf stderr."
title: Schreiben auf Standardfehler
weight: 25
---

## Wie zu:
TypeScript, als eine Obermenge von JavaScript, stützt sich auf die zugrunde liegende JS-Laufzeitumgebung (wie Node.js) für das Schreiben auf stderr. Hier sehen Sie, wie Sie es direkt machen können:

```typescript
console.error("Dies ist eine Fehlermeldung.");
```

Beispielausgabe zu stderr:
```
Dies ist eine Fehlermeldung.
```

In einer Node.js-Umgebung können Sie auch die Methode `process.stderr.write()` für ein tiefergehendes Schreiben verwenden:

```typescript
process.stderr.write("Fehlermeldung auf niedriger Ebene.\n");
```

Beispielausgabe zu stderr:
```
Fehlermeldung auf niedriger Ebene.
```

Für eine strukturiertere Fehlerprotokollierung könnten Sie beliebte Drittanbieter-Bibliotheken wie `winston` oder `pino` verwenden. Hier sehen Sie, wie man Fehler mit `winston` protokolliert:

Zuerst installieren Sie `winston`:

```bash
npm install winston
```

Dann verwenden Sie es in Ihrer TypeScript-Datei:

```typescript
import * as winston from 'winston';

const logger = winston.createLogger({
  levels: winston.config.syslog.levels,
  transports: [
    new winston.transports.Console(),
    new winston.transports.File({ filename: 'error.log', level: 'error' })
  ],
});

logger.error('Fehler mit winston protokolliert.');
```

Dies schreibt den Fehler sowohl auf die Konsole als auch in eine Datei namens `error.log`. Denken Sie daran, beim Schreiben in Dateien ist es wichtig, Dateiberechtigungen und Rollover zu verwalten, um Probleme im Zusammenhang mit der Speicherplatznutzung zu verhindern.
