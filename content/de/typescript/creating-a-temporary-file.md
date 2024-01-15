---
title:                "Erstellen einer temporären Datei"
html_title:           "TypeScript: Erstellen einer temporären Datei"
simple_title:         "Erstellen einer temporären Datei"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/typescript/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Warum

Das Erstellen von temporären Dateien kann in vielen Fällen nützlich sein, wie zum Beispiel beim Testen von Code oder beim Zwischenspeichern von Daten. Es ermöglicht auch die effiziente Nutzung von Systemressourcen und das Vermeiden von Dateikonflikten.

## Wie geht das?

Das Erstellen einer temporären Datei in TypeScript ist sehr einfach. Alles, was Sie brauchen, ist das `fs`-Modul, das in Node.js eingebaut ist.

```TypeScript 
const fs = require('fs'); 
```

Als nächstes können Sie die `fs`-Funktion `mkdtempSync()` verwenden, um eine temporäre Verzeichnisname zu erstellen, in dem Sie Ihre Datei speichern können.

```TypeScript 
const tempDir = fs.mkdtempSync(''); 
```

Durch das Verwenden eines leeren Strings als Argument erstellt die `mkdtempSync()`-Funktion einen zufälligen Verzeichnisnamen, der mit dem Betriebssystem temporären Verzeichnispfad beginnt.

Jetzt können wir den Pfad zu unserer temporären Datei erstellen, indem wir den zuvor erstellten temporären Verzeichnisnamen und den gewünschten Dateinamen kombinieren.

```TypeScript 
const tempFilePath = `${tempDir}/myTempFile.txt`; 
```

Als nächstes können wir den Inhalt unserer temporären Datei erstellen und speichern, indem wir die `fs`-Funktion `writeFileSync()` verwenden.

```TypeScript 
const fileContent = 'Hello world!'; 
fs.writeFileSync(tempFilePath, fileContent); 
```

Und das ist alles! Sie haben erfolgreich eine temporäre Datei erstellt und mit Inhalt gefüllt. Achten Sie jedoch darauf, dass das Verzeichnis und die Datei anschließend gelöscht werden müssen, um Speicherplatz zu sparen und Dateikonflikte zu vermeiden.

## Tief eintauchen

Das `fs`-Modul bietet auch die Möglichkeit, eine temporäre Datei mit einem benutzerdefinierten Präfix zu erstellen. Dies kann nützlich sein, um die temporäre Datei zu identifizieren und von anderen Dateien zu unterscheiden.

```TypeScript 
const tempDir = fs.mkdtempSync('myTempFolder_'); 
const tempFilePath = `${tempDir}/myTempFile.txt`; 
```

Darüber hinaus bietet Node.js die Möglichkeit, temporäre Dateien automatisch zu erstellen und zu löschen, indem man das `os`-Modul verwendet.

```TypeScript 
const os = require('os'); 
const path = require('path'); 

// Erstelle eine temporäre Datei 
const tempFilePath = path.join(os.tmpdir(), 'myTempFile.txt'); 

// Lösche die temporäre Datei nach Verwendung 
fs.unlinkSync(tempFilePath); 
```

## Siehe auch

- [https://nodejs.org/api/fs.html](https://nodejs.org/api/fs.html)
- [https://nodejs.org/api/child_process.html](https://nodejs.org/api/child_process.html)
- [https://nodejs.org/api/os.html](https://nodejs.org/api/os.html)