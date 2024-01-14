---
title:                "TypeScript: Eine temporäre Datei erstellen."
programming_language: "TypeScript"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/typescript/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Warum

Das Erstellen einer temporären Datei kann in vielen Szenarien nützlich sein. Zum Beispiel, wenn man eine temporäre Zwischenspeicherung von Daten benötigt oder wenn man eine Datei mit einer eindeutigen Bezeichnung erstellen muss. In diesem Blog-Beitrag werden wir uns ansehen, wie man in TypeScript eine temporäre Datei erstellt und einige interessante Aspekte des Prozesses vertiefen wird.

## Wie man eine temporäre Datei in TypeScript erstellt

Um in TypeScript eine temporäre Datei zu erstellen, gibt es einige Schritte, die befolgt werden müssen:

Schritt 1: Installieren Sie das Modul "tmp-promise" von npm.

```
yarn add tmp-promise
```

Schritt 2: Importieren Sie das  "tmp-promise" Modul in Ihrer TypeScript-Datei.

```TypeScript
import * as tmp from 'tmp-promise'; 
```

Schritt 3: Verwenden Sie die Funktion "file()" des "tmp"-Moduls, um einen temporären Dateinamen zu generieren.

```TypeScript
let tempFile = await tmp.file();
```

Schritt 4: Schreiben Sie Daten in die temporäre Datei.

```TypeScript
await fs.writeFile(tempFile.path, "Dies ist eine temporäre Datei.", 'utf8');
```

Schritt 5: Löschen Sie die temporäre Datei, wenn sie nicht mehr benötigt wird.

```TypeScript
await fs.unlink(tempFile.path);
```

Das ist es! Mit diesen einfachen Schritten können Sie eine temporäre Datei in Ihrem TypeScript-Code erstellen und verwenden. Nun werden wir uns genauer ansehen, was hinter den Kulissen passiert.

## Deep Dive

Das "tmp-promise" Modul verwendet das Betriebssystem-Modul von Node.js, um einen temporären Dateinamen zu generieren. Dabei wird ein eindeutiger Dateiname mit wenigen zufälligen Zeichen erstellt, um sicherzustellen, dass keine Konflikte mit anderen Dateien auftreten.

Außerdem nutzt das "tmp"-Modul die Promisfizierungsfunktionen von Node.js, was bedeutet, dass Sie "await" verwenden können, um asynchronen Code zu schreiben, anstatt Callbacks zu verwenden.

Ein weiteres interessantes Feature des "tmp"-Moduls ist, dass es die temporäre Datei automatisch löscht, wenn Ihr Programm beendet wird. Auf diese Weise müssen Sie sich keine Gedanken darüber machen, die temporäre Datei manuell zu entfernen.

In der Realität kann es nützlich sein, wenn die temporäre Datei nicht gleich nach dem Erstellen gelöscht wird, sondern erst nachdem sie nicht mehr benötigt wird. In solchen Fällen können Sie die Option "keep" in der Funktion "file()" verwenden.

```TypeScript
let tempFile = await tmp.file({keep: true});
```

Auf diese Weise bleibt die temporäre Datei gespeichert, bis Sie sie manuell löschen oder bis das Programm beendet wird.

## Siehe auch

- [https://www.npmjs.com/package/tmp-promise](https://www.npmjs.com/package/tmp-promise)
- [https://nodejs.org/api/fs.html](https://nodejs.org/api/fs.html)

Vielen Dank, dass Sie sich die Zeit genommen haben, diesen Artikel zu lesen. Wir hoffen, dass Sie nun in der Lage sind, problemlos temporäre Dateien in Ihren TypeScript-Projekten zu erstellen. Happy Coding!