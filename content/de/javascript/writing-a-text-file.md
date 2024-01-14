---
title:                "Javascript: Eine Textdatei schreiben"
simple_title:         "Eine Textdatei schreiben"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/javascript/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Warum

Beim Programmieren gibt es verschiedene Arten, Daten zu speichern. Eine beliebte Methode ist das Schreiben von Textdateien. In diesem Blog-Beitrag werden wir uns anschauen, warum es sinnvoll ist, Textdateien zu schreiben und wie man das in JavaScript macht.

## Wie geht das?

Um eine Textdatei in JavaScript zu schreiben, benötigen wir die File System (fs) Bibliothek. Als Erstes müssen wir diese Bibliothek importieren:

```Javascript
const fs = require('fs');
```

Als Nächstes erstellen wir eine neue Datei oder öffnen eine bestehende Datei. Wir können dazu die Funktion `fs.openSync` verwenden, die als Parameter den Dateinamen und den Schreibmodus benötigt. Der Dateiname kann ein absoluter oder relativer Pfad sein.

```Javascript
const file = fs.openSync('beispiel.txt', 'w');
```

Jetzt können wir die eigentlichen Daten in die Datei schreiben. Dazu nutzen wir die Funktion `fs.writeFileSync`. Diese benötigt ebenfalls den Dateinamen und als zweiten Parameter den Text, den wir schreiben möchten.

```Javascript
fs.writeFileSync('beispiel.txt', 'Hallo Welt!');
```

Wenn wir nun die Datei öffnen, sehen wir den Text "Hallo Welt!".

## Tiefer Einblick

Die `fs` Bibliothek bietet uns viele verschiedene Funktionen zum Lesen und Schreiben von Dateien. Wir können zum Beispiel auch Dateien einlesen und deren Inhalt in eine Variable speichern, anstatt sie direkt in eine Datei zu schreiben.

```Javascript
const text = fs.readFileSync('beispiel.txt', 'utf8');
console.log(text); // Ausgabe: Hallo Welt!
```

Wir können auch Dateien umbenennen oder verschieben und sogar Ordner erstellen oder löschen.

```Javascript
// Umbenennen
fs.renameSync('beispiel.txt', 'neuer_name.txt');

// Verschieben
fs.renameSync('neuer_name.txt', '/pfad/zum/zielordner/neuer_name.txt');

// Ordner erstellen
fs.mkdirSync('neuer_ordner');

// Ordner löschen
fs.rmdirSync('neuer_ordner');
```

Als Entwickler ist es wichtig, auch sicherzustellen, dass unsere Dateien und Ordner existieren, bevor wir mit ihnen arbeiten. Dafür können wir die Funktionen `fs.existsSync` und `fs.existsSync` verwenden.

```Javascript
// Überprüfen, ob Datei existiert
if (fs.existsSync('beispiel.txt')) {
  // Code zum Lesen/Schreiben der Datei
}

// Überprüfen, ob Ordner existiert
if (fs.existsSync('neuer_ordner')) {
  // Code zum Erstellen/Löschen von Dateien in diesem Ordner
}
```

## Siehe auch

- [Node.js fs Dokumentation](https://nodejs.org/api/fs.html)
- [JavaScript Dateien schreiben mit der fs Bibliothek](https://www.digitalocean.com/community/tutorials/node-js-reading-and-writing-files)
- [Tutorial: Dateien mit Node.js lesen und schreiben](https://www.twilio.com/blog/working-with-files-javascript-node-js)