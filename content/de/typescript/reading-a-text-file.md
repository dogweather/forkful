---
title:                "Eine Textdatei lesen"
html_title:           "Bash: Eine Textdatei lesen"
simple_title:         "Eine Textdatei lesen"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/typescript/reading-a-text-file.md"
---

{{< edit_this_page >}}

# Programmierung mit TypeScript: Textdateien lesen

## Was & Warum?
Das Lesen einer Textdatei ist der Prozess, bei dem ein Programm Daten von einer Textdatei in den Speicher lädt. Programmierer nutzen diesen Vorgang oft, um Daten zu analysieren, zu manipulieren oder anderweitig zu verarbeiten.

## So geht's:

In TypeScript können wir das `fs` Modul von Node.js nutzen, um eine Textdatei zu lesen. Hier ist ein einfaches Beispiel:

```TypeScript
var fs = require('fs');

fs.readFile('Beispiel.txt', 'utf8', function(err, data) {
    if (err) throw err;
    console.log(data);
});
```

Dieser Code liest die Datei 'Beispiel.txt' und gibt ihren Inhalt in der Konsole aus. Durch das Hinzufügen des 'utf8'-Parameters lesen wir die Datei als Text.

## Vertiefung

Node.js und das 'fs' Modul existieren seit etwa 2009 und haben sich als Standardwerkzeuge für I/O-Vorgänge in JavaScript und TypeScript etabliert. Es gibt viele Alternativen wie `readline` oder `stream`, die für spezifische Anwendungsfälle besser geeignet sein können.

Das 'fs' Modul basiert auf asynchronen Callbacks, dies bedeutet, dass der Code während des Lesens von Dateien mit anderen Aufgaben fortfahren kann. Das erhöht die Effizienz, besonders bei großen Dateien.

## Siehe Auch

1. Node.js Dokumentation zum 'fs' Modul: https://nodejs.org/api/fs.html
2. TypeScript Tutorials: https://www.typescriptlang.org/docs
3. Umgang mit Dateien in Node.js: https://nodejs.dev/learn/the-nodejs-fs-module