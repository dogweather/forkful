---
title:                "TypeScript: Erstellen einer temporären Datei"
simple_title:         "Erstellen einer temporären Datei"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/typescript/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Warum

Das Erstellen von temporären Dateien ist oft eine notwendige Aufgabe in der Programmierung. Es kann hilfreich sein, um Daten temporär zu speichern, bevor sie in eine dauerhafte Datei geschrieben werden, um Verwechslungen mit bereits bestehenden Dateien zu vermeiden oder um temporäre Dateien für Testzwecke zu verwenden.

## Wie

Um eine temporäre Datei in TypeScript zu erstellen, können wir die Funktion `createTemporaryFile()` aus dem `fs`-Modul verwenden. Sie nimmt drei Argumente an: einen Präfix, einen Suffix und eine optionale callback-Funktion.

```TypeScript
import * as fs from "fs";

fs.createTemporaryFile("prefix_", ".txt", (err, fileName) => {
  if (err) throw err;
  console.log(`${fileName} wurde erfolgreich erstellt!`);
});
```

Dieser Code erstellt eine temporäre Datei mit dem Präfix "prefix_" und dem Suffix ".txt". Als Ergebnis erhalten wir den Dateinamen der erstellten temporären Datei, z.B. "prefix_11324.txt".

## Deep Dive

Beim Erstellen einer temporären Datei gibt es einige Dinge zu beachten. Zunächst ist es wichtig, sicherzustellen, dass der Name der erstellten Datei eindeutig ist, um Konflikte mit bereits bestehenden Dateien zu vermeiden. Hier können wir den `tmp`-Namen-Generator aus dem `generate-tmp-filename` npm-Paket verwenden. Dieser Generator generiert einen zufälligen Dateinamen mit der angegebenen Präfix- und Suffix-Struktur.

```TypeScript
import * as fs from "fs";
import * as genTmpName from "generate-tmp-filename";

const fileName = genTmpName("prefix_", ".txt");
fs.createTemporaryFile(fileName, (err) => {
  if (err) throw err;
  console.log(`${fileName} wurde erfolgreich erstellt!`);
});
```

Darüber hinaus ist es auch wichtig, sicherzustellen, dass die erstellte temporäre Datei am Ende der Verwendung gelöscht wird. Dafür können wir die `unlink()`-Funktion aus dem `fs`-Modul verwenden.

```TypeScript
import * as fs from "fs";
import * as genTmpName from "generate-tmp-filename";

const fileName = genTmpName("prefix_", ".txt");
fs.createTemporaryFile(fileName, (err) => {
  if (err) throw err;
  console.log(`${fileName} wurde erfolgreich erstellt!`)
  fs.unlink(fileName, (err) => {
    if (err) throw err;
    console.log(`${fileName} wurde erfolgreich gelöscht.`);
  });
});
```

## Siehe auch

- npm-Paket "generate-tmp-filename": https://www.npmjs.com/package/generate-tmp-filename
- Node.js `fs`-Modul Dokumentation: https://nodejs.org/api/fs.html