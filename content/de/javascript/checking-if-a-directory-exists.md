---
title:                "Javascript: Überprüfung, ob ein Verzeichnis vorhanden ist"
programming_language: "Javascript"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/javascript/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Warum

Das Überprüfen, ob ein Verzeichnis existiert, ist ein wichtiger Schritt in der Entwicklung von JavaScript-Programmen. Es ermöglicht es uns, zu überprüfen, ob bestimmte Dateien oder Ordner vorhanden sind, bevor wir versuchen, auf sie zuzugreifen. Dadurch können wir Fehler vermeiden und unsere Programmabläufe effizienter gestalten.

## Wie man überprüft, ob ein Verzeichnis existiert

Um zu überprüfen, ob ein Verzeichnis existiert, können wir die `fs`-Bibliothek von Node.js verwenden. Wir importieren sie mit dem Befehl `const fs = require('fs')`. Dann können wir die Funktion `fs.existsSync()` verwenden, um zu überprüfen, ob das Verzeichnis vorhanden ist. Die Funktion gibt `true` zurück, wenn das Verzeichnis existiert, und `false`, wenn es nicht vorhanden ist.

```Javascript
const fs = require('fs');
const directory = './myDirectory';

if (fs.existsSync(directory)) {
  console.log(`${directory} existiert!`);
} else {
  console.log(`${directory} existiert nicht.`);
}
```

### Beispiel Output:

```Javascript
./myDirectory existiert!
```

## Tiefer Einblick

Wenn wir uns näher mit der `fs.existsSync()`-Funktion befassen, sehen wir, dass sie den Pfad zu einem Dateisystemobjekt als Parameter erwartet. Dies kann ein Verzeichnis oder eine Datei sein. Die Funktion verwendet den synchronen Aufruf, was bedeutet, dass die Ausführung des Codes blockiert wird, bis die Funktion abgeschlossen ist. Dies kann in manchen Fällen zu einer längeren Verarbeitungszeit führen und sollte bei der Verwendung beachtet werden.

Es ist auch wichtig zu beachten, dass die Funktion `fs.existsSync()` keine Überprüfung auf die Berechtigungen oder Eigentümer der Datei oder des Verzeichnisses durchführt. Sie gibt lediglich an, ob das Verzeichnis oder die Datei existieren oder nicht.

## Siehe auch

- Node.js fs Modul Dokumentation: https://nodejs.org/dist/latest-v14.x/docs/api/fs.html
- Kontrolle, ob eine Datei oder ein Verzeichnis in Javascript existiert: https://www.w3docs.com/snippets/javascript/how-to-check-if-a-file-directory-exists.html
- Verzeichnis- und Dateienbenutzungsstatistik mit fs.stat in Node.js: https://www.section.io/engineering-education/how-to-check-if-directory-exists-in-node-js/