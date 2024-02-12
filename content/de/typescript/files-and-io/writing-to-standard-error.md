---
title:                "Schreiben auf Standardfehler"
aliases:
- /de/typescript/writing-to-standard-error/
date:                  2024-02-03T19:34:35.149980-07:00
model:                 gpt-4-0125-preview
simple_title:         "Schreiben auf Standardfehler"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/typescript/writing-to-standard-error.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Was & Warum?
In TypeScript ist das Schreiben auf Standardfehler (stderr) ein Prozess des Sendens von Fehlermeldungen oder Protokollen direkt an den Fehlerausgabestrom der Umgebung (z.B. die Konsole in node.js oder ein Webbrowser). Dies ist wesentlich für die Diagnose von Problemen, ohne dass der Standardausgabe (stdout) eingegriffen wird, die üblicherweise für Programmdaten verwendet wird. Dies stellt sicher, dass Fehlerbehandlung und Protokollierung effizient und kohärent verwaltet werden.

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
