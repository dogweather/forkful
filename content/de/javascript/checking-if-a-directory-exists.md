---
title:    "Javascript: Überprüfen, ob ein Verzeichnis vorhanden ist"
keywords: ["Javascript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/javascript/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Warum

Das Überprüfen, ob ein Verzeichnis existiert, ist ein wichtiger Teil des Programmierens in Javascript. Es ermöglicht es uns, sicherzustellen, dass wir auf die erforderlichen Dateien und Ordner zugreifen können, bevor wir versuchen, mit ihnen zu arbeiten. Diese Funktion ist besonders nützlich, wenn wir Anwendungen entwickeln, die mit vielen verschiedenen Dateien und Verzeichnissen arbeiten.

## Wie man das macht

Es gibt mehrere Möglichkeiten, um herauszufinden, ob ein Verzeichnis existiert. Eine Möglichkeit ist die Verwendung der `fs`-Bibliothek von Node.js. Hier ist ein Beispielcode, der überprüft, ob das Verzeichnis "Bilder" existiert:

```Javascript
const fs = require('fs');
const path = './Bilder';

if (fs.existsSync(path)) {
  console.log('Das Verzeichnis existiert!');
} else {
  console.log('Das Verzeichnis existiert nicht!');
}
```

Die Ausgabe dieses Codes wird je nachdem, ob das Verzeichnis existiert oder nicht, unterschiedlich sein. Wenn das Verzeichnis existiert, sehen wir "Das Verzeichnis existiert!" in unserer Konsole, andernfalls sehen wir "Das Verzeichnis existiert nicht!".

Eine andere Möglichkeit ist die Verwendung der `fs.stat()`-Funktion, um Informationen über das Verzeichnis abzurufen. Hier ist ein Beispielcode, der prüft, ob das Verzeichnis "Musik" existiert:

```Javascript
const fs = require('fs');
const path = './Musik';

fs.stat(path, (err, stats) => {
  if (err) {
    console.log('Das Verzeichnis existiert nicht!');
  } else {
    console.log('Das Verzeichnis existiert!');
  }
});
```

Wiederum sehen wir je nach Ergebnis "Das Verzeichnis existiert!" oder "Das Verzeichnis existiert nicht!" in unserer Konsole.

## Tiefere Einblicke

Es ist wichtig zu beachten, dass bei der Überprüfung, ob ein Verzeichnis existiert, unterschiedliche Ergebnisse angezeigt werden können, je nachdem, welche Pfade angegeben werden. Zum Beispiel kann das Verzeichnis "Bilder" im gleichen Ordner wie unser Javascript-Code vorhanden sein oder in einem anderen Ordner auf unserem Computer. Dies kann zu unerwarteten Ergebnissen führen, wenn wir nicht genau auf die Pfadangaben achten.

Außerdem ist es ratsam, bei der Entwicklung von Anwendungen, die mit Dateisystemen arbeiten, immer sicherzustellen, dass die Dateien und Verzeichnisse, auf die wir zugreifen wollen, auch tatsächlich existieren, um Fehler und unerwartetes Verhalten zu vermeiden.

## Siehe auch

- [Node.js `fs`-Dokumentation](https://nodejs.org/api/fs.html)
- [Überprüfen, ob eine Datei existiert in Javascript](https://www.freecodecamp.org/news/check-if-a-file-exists-in-node-js/)
- [Arbeiten mit Dateien und Verzeichnissen in Node.js](https://www.digitalocean.com/community/tutorials/nodejs-working-with-files)