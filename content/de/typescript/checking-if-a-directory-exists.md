---
title:    "TypeScript: Es existiert kein Verzeichnis."
keywords: ["TypeScript"]
---

{{< edit_this_page >}}

## Warum

Eine der grundlegenden Aufgaben eines Programmierers ist es, sicherzustellen, dass das Programm auf alle möglichen Situationen vorbereitet ist. Eine solche Situation kann das Vorhandensein oder Nichtvorhandensein eines Verzeichnisses sein. Deshalb ist es wichtig zu wissen, wie man überprüft, ob ein Verzeichnis existiert, bevor man versucht, darauf zuzugreifen.

## How To

Das Überprüfen der Existenz eines Verzeichnisses in TypeScript ist relativ einfach. Wir können die Funktion `fs.existsSync()` aus dem Node.js-Modul "fs" verwenden, um dies zu tun. Schauen wir uns ein Beispiel an:

```TypeScript
const fs = require('fs');

// Überprüfung des Verzeichnisses "exampleDir"
if (fs.existsSync('exampleDir')) {
  console.log('Das Verzeichnis "exampleDir" existiert!');
} else {
  console.log('Das Verzeichnis "exampleDir" existiert nicht!');
}
```

Die `existsSync()`-Funktion gibt entweder `true` oder `false` zurück, je nachdem, ob das angegebene Verzeichnis existiert oder nicht. Dies ermöglicht es uns, bedingte Anweisungen zu verwenden, um auf unterschiedliche Szenarien zu reagieren.

## Deep Dive

Im Hintergrund ruft `existsSync()` die `fs.statSync()`-Funktion auf, um die Statistiken des angegebenen Pfades abzurufen. Diese Statistiken enthalten Informationen wie den Dateityp, die Größe und den Zeitstempel der letzten Bearbeitung. Wenn das übergebene Verzeichnis nicht existiert, wird eine Ausnahme ausgelöst, die von `existsSync()` abgefangen wird und `false` zurückgibt.

Es ist zu beachten, dass `existsSync()` nicht nur für Verzeichnisse funktioniert, sondern auch für Dateien und symbolische Links verwendet werden kann.

## Siehe auch

- [Node.js Dokumentation: "fs" Modul](https://nodejs.org/api/fs.html)
- [Node.js Dokumentation: "fs.existsSync()"](https://nodejs.org/api/fs.html#fs_fs_existssync_path)