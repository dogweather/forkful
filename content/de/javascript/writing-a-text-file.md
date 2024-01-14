---
title:    "Javascript: Das Verfassen einer Textdatei"
keywords: ["Javascript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/javascript/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Warum
Das Schreiben von Textdateien ist eine grundlegende Fähigkeit für jeden Programmierer. Es ermöglicht uns, Informationen in einem einfachen Format zu speichern und zu teilen, das von verschiedenen Programmen gelesen werden kann. In diesem Blogbeitrag werden wir uns genauer mit dem Schreiben von Textdateien in JavaScript befassen.

## Wie
Um eine Textdatei in JavaScript zu schreiben, müssen wir zunächst ein Objekt erstellen, das mit der Datei verbunden ist. Dies können wir mit der `fs`-Bibliothek von Node.js tun. Dann können wir die `writeFile()`-Funktion verwenden, um den Inhalt in die Datei zu schreiben. Schauen wir uns ein Beispiel an:

```Javascript
const fs = require('fs');

// Erstelle ein Dateiobjekt und gebe den Dateinamen an
const datei = 'neue_datei.txt';

// Schreibe den gewünschten Inhalt in die Datei
fs.writeFile(datei, 'Dies ist der Inhalt meiner Datei.', (error) => {
  if (error) {
    console.log(error);
  } else {
    console.log('Datei erfolgreich erstellt.');
  }
});
```

Die `writeFile()`-Funktion nimmt als Parameter den Dateinamen, den Inhalt der Datei und eine callback-Funktion entgegen. Diese callback-Funktion wird aufgerufen, sobald die Datei erstellt wurde. Wenn ein Fehler auftritt, wird dieser in der Konsole ausgegeben.

Die oben genannte Funktion kann auch verwendet werden, um bereits vorhandene Dateien zu überschreiben. Wenn Sie jedoch eine Datei hinzufügen möchten, anstatt sie zu überschreiben, können Sie die `appendFile()`-Funktion verwenden.

```Javascript
// Füge zusätzlichen Inhalt in die Datei hinzu
fs.appendFile(datei, 'Dieser Text wird der Datei hinzugefügt.', (error) => {
  if (error) {
    console.log(error);
  } else {
    console.log('Text erfolgreich hinzugefügt.');
  }
});
```

## Deep Dive
Ein wichtiger Punkt beim Schreiben von Textdateien in JavaScript ist, dass es sich bei der `writeFile()`-Funktion um einen asynchronen Vorgang handelt. Das bedeutet, dass das Programm nicht auf die Fertigstellung des Schreibvorgangs wartet, sondern mit der Ausführung des restlichen Codes fortsetzt. Wenn Sie sicherstellen möchten, dass der Schreibvorgang abgeschlossen ist, bevor Sie fortfahren, können Sie ein `callback` nutzen oder `promises` verwenden.

Außerdem ist es wichtig zu beachten, dass der Inhalt der Datei als `String` angegeben werden muss. Wenn Sie also Objekte oder Arrays in die Datei schreiben möchten, müssen Sie diese in einen `String` konvertieren.

## Siehe auch
- Die `fs`-Bibliothek in der Node.js-Dokumentation: https://nodejs.org/api/fs.html
- Ein Tutorial zum Schreiben von Textdateien in JavaScript: https://www.freecodecamp.org/news/node-js-write-to-file/