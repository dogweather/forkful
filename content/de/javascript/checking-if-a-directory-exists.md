---
title:                "Javascript: Überprüfung, ob ein Verzeichnis existiert"
simple_title:         "Überprüfung, ob ein Verzeichnis existiert"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/javascript/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

# Warum

Das Überprüfen, ob ein Verzeichnis existiert, kann in der Javascript-Programmierung äußerst nützlich sein, insbesondere wenn man mit einer Dateistruktur arbeitet. Indem man sicherstellt, dass ein bestimmtes Verzeichnis vorhanden ist, kann man sicherstellen, dass der Code fehlerfrei ausgeführt wird und möglicherweise unerwartete Abstürze verhindern.

# Anleitung

Die Überprüfung, ob ein Verzeichnis existiert, kann mit Hilfe der `fs` (FileSystem) Modul von Node.js durchgeführt werden. Zunächst müssen wir das Modul mit `require` importieren:

```Javascript
const fs = require('fs');
```

Dann können wir die Funktion `fs.existsSync()` verwenden, um zu überprüfen, ob ein Verzeichnis mit einem angegebenen Pfad vorhanden ist. Hier ist ein Beispielcode:

```Javascript
const path = './meinVerzeichnis';

// Überprüfe ob das Verzeichnis existiert
if (fs.existsSync(path)) {
    console.log('Das Verzeichnis existiert.');
} else {
    console.log('Das Verzeichnis existiert nicht.');
}
```

Die Ausgabe wird je nachdem, ob das Verzeichnis tatsächlich vorhanden ist oder nicht, unterschiedlich sein. Wenn das Verzeichnis existiert, wird `"Das Verzeichnis existiert."` ausgegeben, ansonsten wird `"Das Verzeichnis existiert nicht."` ausgegeben.

# Tiefere Einblicke

Wenn man tiefer in das Thema einsteigen möchte, sollte man sich mit der Struktur der Dateisysteme und den verschiedenen Dateiformaten vertraut machen. Auch das Verständnis von Fehlerbehandlung ist wichtig, um effektiv auf das Ergebnis der Überprüfung zu reagieren.

Es ist auch erwähnenswert, dass die `fs.existsSync()` Funktion nur überprüft, ob das Verzeichnis existiert. Sie gibt kein Feedback darüber, ob das angegebene Objekt tatsächlich ein Verzeichnis ist. Um dies herauszufinden, kann man die `fs.lstatSync()` Funktion verwenden, die Informationen über das angegebene Objekt zurückgibt.

# Siehe auch

- [`fs` Modul in der Node.js Dokumentation](https://nodejs.org/api/fs.html)
- [Einführung in das Dateisystem in Node.js](https://www.w3schools.com/nodejs/nodejs_filesystem.asp)
- [Guide zur Fehlerbehandlung in Node.js](https://itnext.io/error-handling-in-node-js-713eda06a312)