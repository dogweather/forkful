---
title:                "Überprüfen, ob ein Verzeichnis existiert"
html_title:           "TypeScript: Überprüfen, ob ein Verzeichnis existiert"
simple_title:         "Überprüfen, ob ein Verzeichnis existiert"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/typescript/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Warum

Eine häufige Aufgabe beim Entwickeln von Anwendungen ist das Überprüfen, ob ein Verzeichnis existiert. Dies kann sowohl für die Strukturierung von Dateien als auch für die Fehlerbehandlung von großer Bedeutung sein.

## Wie es geht

In TypeScript können wir mithilfe der `fs`-Bibliothek überprüfen, ob ein Verzeichnis existiert. Hier ist ein einfaches Beispiel, das zeigt, wie wir vorgehen können:

```TypeScript
import fs from 'fs';

const dirName = 'my-directory';

if (fs.existsSync(dirName)) {
  console.log(`${dirName} existiert.`);
} else {
  console.log(`${dirName} existiert nicht.`);
}
```

Im obigen Beispiel importieren wir die `fs`-Bibliothek und verwenden die `existsSync()`-Methode, um zu überprüfen, ob das Verzeichnis mit dem angegebenen Namen existiert. Wenn dies der Fall ist, wird die entsprechende Meldung ausgegeben, andernfalls wird eine andere Meldung ausgegeben.

## Tiefere Einblicke

Die `existsSync()`-Methode ist Teil der `fs`-Bibliothek und gibt `true` zurück, wenn das Verzeichnis existiert, andernfalls gibt sie `false` zurück.

Es ist auch wichtig zu beachten, dass dieser Ansatz nur das Vorhandensein eines Verzeichnisses überprüft, nicht jedoch ob es eine tatsächliche Datei ist. Wenn wir überprüfen möchten, ob es sich um eine Datei handelt, können wir die `fs.statSync()`-Methode verwenden.

Eine weitere Möglichkeit, um eine Überprüfung auf ein Verzeichnis durchzuführen, besteht darin, die `fs.accessSync()`-Methode zu verwenden. Diese Methode ermöglicht es uns, verschiedene Arten von Berechtigungen zu überprüfen, wie z.B. die Rechte zum Lesen, Schreiben oder Ausführen von Dateien oder Verzeichnissen.

## Siehe auch

- [Node.js - fs Modul Dokumentation](https://nodejs.org/api/fs.html)
- [TypeScript Dokumentation - Dateisystem](https://www.typescriptlang.org/docs/handbook/file-system.html)