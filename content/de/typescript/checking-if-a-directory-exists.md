---
title:                "Überprüfung, ob ein Verzeichnis existiert"
html_title:           "Lua: Überprüfung, ob ein Verzeichnis existiert"
simple_title:         "Überprüfung, ob ein Verzeichnis existiert"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/typescript/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

# Ein Blick auf die Directory Existenzüberprüfung in TypeScript

## Was und Warum?
Die Überprüfung, ob ein Verzeichnis existiert, ist ein Verfahren in der Programmierung, bei dem wir feststellen, ob ein Verzeichnis (ein Ordner auf der Festplatte) vorhanden ist oder nicht. Wir machen dies, um Fehler zu vermeiden, die auftreten können, wenn wir versuchen, auf ein nicht existierendes Verzeichnis zuzugreifen.

## So geht's:

In TypeScript prüfen wir die Existenz eines Verzeichnisses mit dem File System (fs) Modul, welches in Node.js eingebaut ist. Wir verwenden die `existsSync()` Funktion, die ein boolean zurückgibt.

```TypeScript
import * as fs from 'fs';

let dirPath: string = '/pfad/zum/verzeichnis';
if (fs.existsSync(dirPath)) {
  console.log('Das Verzeichnis existiert.');
} else {
  console.log('Das Verzeichnis existiert nicht.');
}
```

Auf Ihrem Terminal werden Sie dann sehen:

```Terminal
Das Verzeichnis existiert.
```
oder
```Terminal
Das Verzeichnis existiert nicht.
```

## Vertiefung

Historisch gesehen haben Programmierer oft Zugriffsfehler bekommen, weil sie versucht haben, auf ein nicht existierendes Verzeichnis zuzugreifen. Daher ist die Existenzüberprüfung ein unabdingbarer Bestandteil in der Programmierung geworden.

Alternativ können wir auch die `access()` Funktion innerhalb des fs Moduls verwenden, die mehr Einzelheiten über den Zugriff liefert.

```TypeScript
import * as fs from 'fs';

let dirPath: string = '/pfad/zum/verzeichnis';
fs.access(dirPath, fs.constants.F_OK, (err) => {
  console.log(`${dirPath} ${err ? 'existiert nicht.' : 'existiert.'}`);
});
```

Diese Implementierung ist etwas komplexer, bietet jedoch mehr Flexibilität.

## Siehe auch

1. Node.js fs Modul Dokumentation: [Link](https://nodejs.org/api/fs.html)
2. TypeScript Dokumentation: [Link](https://www.typescriptlang.org/docs/) 

Mit diesen Werkzeugen ausgestattet, sollten Sie bereit sein, sicher mit Verzeichnissen in TypeScript zu arbeiten.