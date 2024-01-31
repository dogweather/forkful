---
title:                "Überprüfung, ob ein Verzeichnis existiert"
date:                  2024-01-20T14:57:19.026785-07:00
html_title:           "Fish Shell: Überprüfung, ob ein Verzeichnis existiert"
simple_title:         "Überprüfung, ob ein Verzeichnis existiert"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/javascript/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Was & Warum?
Wir prüfen, ob ein Verzeichnis existiert, um Datenverlust zu vermeiden oder sicherzustellen, dass unsere Programme korrekt auf Dateisysteme zugreifen können. Es ist wichtig, vor dem Lesen, Schreiben oder Modifizieren von Dateien die Existenz zu bestätigen.

## How to:
In JavaScript (Node.js-Umgebung) überprüfen wir, ob ein Verzeichnis existiert, indem wir das `fs`-Modul für den Zugriff auf das Dateisystem verwenden.

```javascript
const fs = require('fs');
const path = './meinVerzeichnis';

try {
  if (fs.existsSync(path)) {
    console.log('Das Verzeichnis existiert.');
  } else {
    console.log('Das Verzeichnis existiert nicht.');
  }
} catch(err) {
  console.error(err);
}
```
Sample Output:
```
Das Verzeichnis existiert.
```

## Deep Dive:
Historisch gesehen gab es verschiedene Methoden, um die Existenz eines Verzeichnisses zu prüfen. Früher wurde oft `fs.exists()` verwendet, aber diese Methode wurde aufgrund ihrer unkonventionellen Fehlerbehandlung als veraltet betrachtet. Das Verwenden von `fs.existsSync()` ist blockierend, d.h., es wartet, bis eine Antwort zurückkommt, bevor es fortfährt. Für nicht-blockierende, asynchrone Operationen sollte man `fs.promises.access()` mit `fs.constants.F_OK` kombinieren.

Alternative Methode:
```javascript
const fsPromises = require('fs').promises;

async function checkDirectoryExists(dir) {
  try {
    await fsPromises.access(dir, fs.constants.F_OK);
    console.log('Das Verzeichnis existiert.');
  } catch {
    console.log('Das Verzeichnis existiert nicht.');
  }
}

checkDirectoryExists('./meinVerzeichnis');
```
Beim historischen Kontext ist es wichtig zu wissen, dass die Art, wie Node.js mit dem Dateisystem interagiert, eng mit POSIX-Systemaufrufen zusammenhängt. Viele `fs`-Methoden bieten eine direkte Schnittstelle zu diesen systemnahen Operationen.

## See Also:
- Node.js Dokumentation zum `fs`-Modul: [Node.js fs module](https://nodejs.org/api/fs.html)
- POSIX Systemaufrufe Referenz: [POSIX calls](https://pubs.opengroup.org/onlinepubs/9699919799/functions/contents.html)
