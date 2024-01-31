---
title:                "Eine Textdatei schreiben"
date:                  2024-01-19
html_title:           "Arduino: Eine Textdatei schreiben"
simple_title:         "Eine Textdatei schreiben"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/typescript/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Was & Warum?
Das Schreiben einer Textdatei ermöglicht die dauerhafte Speicherung von Daten. Es wird gebraucht, um Konfigurationen, Log-Daten oder Benutzerinhalte zu sichern.

## How to:
Installiere und importiere die `fs` Bibliothek für File-System-Operationen.

```TypeScript
import { writeFile } from 'fs';

const content: string = 'Hallo Welt!';

writeFile('example.txt', content, (err) => {
  if (err) {
    console.error('Ein Fehler ist aufgetreten:', err);
    return;
  }
  console.log('Datei wurde geschrieben!');
});
```

Ausgabe beim erfolgreichen Schreiben:

```
Datei wurde geschrieben!
```

## Deep Dive
In den frühen Tagen der Informatik wurden Daten meist auf Bändern gespeichert. Heute gibt es viele Möglichkeiten, Daten zu speichern, wie z.B. Datenbanken und Cloud-Services. Textdateien sind einfach und universell einsetzbar, was sie trotzdem relevant hält. Node.js, die Laufzeitumgebung, benutzt das `fs` Modul, um Zugriff auf das Dateisystem zu erhalten.

## See Also
- Node.js `fs` Module Dokumentation: https://nodejs.org/api/fs.html
- TypeScript Dokumentation: https://www.typescriptlang.org/docs/
- Überblick über Dateisystem-Interaktion in Node.js: https://nodejs.dev/learn/the-nodejs-fs-module
