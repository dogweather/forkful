---
title:    "Javascript: Erstellen einer temporären Datei"
keywords: ["Javascript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/javascript/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Warum

Das Erstellen von temporären Dateien ist eine häufige Aufgabe in der Javascript-Programmierung. Es bietet eine einfache Möglichkeit, Daten temporär zu speichern, ohne eine permanente Datei zu erstellen. Dies kann besonders nützlich sein, wenn Daten nur vorübergehend benötigt werden und danach nicht mehr gebraucht werden.

## Wie es geht

Die Erstellung einer temporären Datei in Javascript ist ein einfacher Vorgang. Zunächst müssen wir das `fs`-Modul von Node.js importieren und den Pfad der temporären Datei angeben:

```javascript
const fs = require('fs');
const tempFilePath = './temp/tempFile.txt';
```

Als nächstes können wir die Datei mit dem `createWriteStream()`-Befehl erstellen und mit Inhalten füllen:

```javascript
const writeStream = fs.createWriteStream(tempFilePath);
writeStream.write('Dies ist ein Beispieltext.');
```

Schließlich schließen wir den Stream und löschen die temporäre Datei mit dem `unlink()`-Befehl:

```javascript
writeStream.end();
fs.unlink(tempFilePath, (err) => {
  if (err) throw err;
  console.log('Temporäre Datei wurde erfolgreich gelöscht.');
});
```

Die Ausgabe des obigen Codes wäre:

```
Temporäre Datei wurde erfolgreich gelöscht.
```

## Tiefer Eintauchen

Beim Erstellen einer temporären Datei gibt es einige wichtige Dinge zu beachten. Erstens müssen wir sicherstellen, dass die temporäre Datei nicht bereits existiert, bevor wir sie erstellen. Darüber hinaus können wir auch angeben, welche Art von Operationen auf der Datei ausgeführt werden können, z. B. nur Schreibvorgänge oder sowohl Lese- als auch Schreibvorgänge.

Um dies zu tun, können wir den `fs.constants` verwenden und die gewünschten Operationen angeben:

```javascript
const tempFilePath = './temp/tempFile.txt';
const flags = fs.constants.O_CREAT | fs.constants.O_WRONLY;

const writeStream = fs.createWriteStream(tempFilePath, { flags });
```

Weitere Informationen zum Regelwerk der `fs.constants` können in der [Node.js Dokumentation](https://nodejs.org/api/fs.html#fs_file_system_flags) gefunden werden.

## Siehe auch

Für weitere Informationen zum Erstellen temporärer Dateien in Javascript empfehlen wir die folgenden Ressourcen:

- [Creating Temporary Files in Node.js - geeksforgeeks.org](https://www.geeksforgeeks.org/creating-temporary-files-node-js/)
- [The tmp Package - npmjs.com](https://www.npmjs.com/package/tmp)
- [Node.js FileSystem Module Documentation - nodejs.org](https://nodejs.org/api/fs.html)

Wir hoffen, dass dieser Beitrag Ihnen geholfen hat, das Konzept des Erstellens temporärer Dateien in der Javascript-Programmierung besser zu verstehen. Viel Spaß beim Coden!