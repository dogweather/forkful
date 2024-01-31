---
title:                "Eine Textdatei schreiben"
date:                  2024-01-19
html_title:           "Arduino: Eine Textdatei schreiben"
simple_title:         "Eine Textdatei schreiben"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/javascript/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Was & Warum?

Das Schreiben einer Textdatei ermöglicht es, Daten persistent zu speichern. Programmierer nutzen dies für Logs, Datenexporte und Konfigurationen.

## So geht's:

Um in JavaScript eine Datei zu schreiben, nutzen wir die `fs`-Bibliothek in Node.js.

```Javascript
const fs = require('fs');

// Text in eine neue Datei schreiben
fs.writeFile('beispiel.txt', 'Hallo, Welt!', function(err) {
    if(err) throw err;
    console.log('Datei wurde gespeichert!');
});

// Text an eine Datei anhängen
fs.appendFile('beispiel.txt', '\nTschüss, Welt!', function(err) {
    if(err) throw err;
    console.log('Text wurde angehängt!');
});
```

## Tiefgang

Das Dateisystemmodul `fs` in Node.js wird seit den Anfängen von Node für Dateioperationen genutzt. Alternativen wie `fs-extra` bieten erweiterte Funktionen. Beim Schreiben sollten mögliche Fehler (z.B. kein Schreibzugriff) behandelt werden.

## Siehe auch

- Node.js Dokumentation zum `fs` Modul: https://nodejs.org/api/fs.html
- `fs-extra` Module: https://www.npmjs.com/package/fs-extra
- MDN Web Docs zum Umgang mit Dateien in JavaScript im Browser: https://developer.mozilla.org/en-US/docs/Web/API/File_and_Directory_Entries_API/Introduction
