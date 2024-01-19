---
title:                "Eine temporäre Datei erstellen"
html_title:           "Java: Eine temporäre Datei erstellen"
simple_title:         "Eine temporäre Datei erstellen"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/javascript/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

# Erstellen Sie eine temporäre Datei in JavaScript

## Was und Warum?

Wir erstellen temporäre Dateien, um Daten vorübergehend zu speichern. Dies ist besonders nützlich bei großen Datenmengen, bei denen die Speicherung im Hauptspeicher nicht geeignet wäre.

## So geht's:

Um eine temporäre Datei in Node.js zu erstellen, verwenden wir das `tmp`-Modul. Installieren Sie es mit `npm install tmp`.

```Javascript
const tmp = require('tmp');

tmp.file({ prefix: 'tmp-', postfix: '.txt' }, function _tempFileCreated(err, path, fd) {
  if (err) throw err;
  
  console.log('Temporary file: ', path);
});
```
Ausgabe:
```
Temporary file:  /tmp/tmp-1234.txt
```

## Tiefere Einblicke:

Historisch gesehen wurden temporäre Dateien verwendet, um bei Ressourcenintensiven Operationen die Leistung zu verbessern. Heute werden sie auch genutzt, um Arbeiten zu isolieren und die Wiederherstellung nach Fehlern zu erleichtern.

Es gibt Alternativen wie In-Memory-Datenbanken (z.B. Redis), aber diese können kostspieliger sein und eignen sich nicht immer für alle Anwendungsfälle.

Die Erstellung einer temporären Datei über das `tmp`-Modul ist sehr einfach: es erstellt die Datei und gibt Ihnen den Pfad zurück. Sie sind dann frei, mit dieser Datei zu tun, was Sie möchten.

## Siehe Auch:

- [tmp Modul Dokumentation](https://www.npmjs.com/package/tmp)
- [File System Modul in Node.js](https://nodejs.org/api/fs.html)
- [Arbeiten mit Dateien in Node.js](https://nodejs.dev/learn/the-nodejs-fs-module)