---
title:                "TypeScript: Prüfen, ob ein Verzeichnis existiert"
simple_title:         "Prüfen, ob ein Verzeichnis existiert"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/typescript/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

# Warum: Die Wichtigkeit des Überprüfens des Existierens eines Verzeichnisses

Das Überprüfen, ob ein Verzeichnis existiert, ist ein wichtiger Schritt in der Programmierung, der dabei helfen kann, Fehler zu vermeiden und sicherzustellen, dass das gewünschte Verzeichnis vorhanden ist, bevor es verwendet wird.

# How To: Überprüfen eines Verzeichnisses in TypeScript

Um ein Verzeichnis in TypeScript zu überprüfen, können wir die `fs` Bibliothek verwenden und die `existsSync()` Methode aufrufen. Diese Methode gibt `true` zurück, wenn das Verzeichnis vorhanden ist, und `false`, wenn es nicht vorhanden ist.

```TypeScript
import * as fs from "fs";

const directory = "/pfad/zum/verzeichnis";

if (fs.existsSync(directory)) {
  console.log("Das Verzeichnis existiert.");
} else {
  console.log("Das Verzeichnis existiert nicht.");
}
```

Das obige Beispiel wird "Das Verzeichnis existiert." ausgeben, wenn das Verzeichnis vorhanden ist, oder "Das Verzeichnis existiert nicht." ausgeben, wenn es nicht vorhanden ist.

# Deep Dive: Weitere Informationen zum Überprüfen von Verzeichnissen

Beim Überprüfen von Verzeichnissen gibt es einige wichtige Details zu beachten:

- Die `existsSync()` Methode erfordert den absoluten Pfad zum Verzeichnis, nicht den relativen Pfad.
- Wenn das Verzeichnis nicht vorhanden ist, wird `false` zurückgegeben, aber wenn ein Fehler auftritt, kann die Methode eine Exception auslösen.
- Wir können auch das `accessSync()` verwenden, um die Zugriffsrechte des Verzeichnisses zu überprüfen, um weitere Kontrolle zu haben.

# Siehe auch

- [Node.js fs Bibliothek Dokumentation](https://nodejs.org/dist/latest-v14.x/docs/api/fs.html)
- [Node.js File System Tutorial auf deutsch](https://www.tutorialspoint.com/nodejs/nodejs_file_system.htm)
- [Offizielle TypeScript Dokumentation](https://www.typescriptlang.org/docs/)