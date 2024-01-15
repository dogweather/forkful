---
title:                "Eine Textdatei schreiben."
html_title:           "Javascript: Eine Textdatei schreiben."
simple_title:         "Eine Textdatei schreiben."
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/javascript/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Warum

Als JavaScript-EntwicklerIn kann es manchmal erforderlich sein, Textdateien zu erstellen und zu bearbeiten. Vielleicht möchten Sie Daten aus einer Anwendung in einem lesbareren Format speichern oder eine benutzerdefinierte Konfigurationsdatei erstellen. In diesem Artikel werden wir uns ansehen, wie Sie das mit JavaScript machen können.

## Wie geht's?

Um eine Textdatei mit JavaScript zu erstellen, müssen Sie zuerst eine Datei-System-Modul importieren. Dazu fügen Sie folgende Zeile hinzu:

```Javascript
const fs = require('fs');
```

Dann können Sie eine neue Textdatei mit dem folgenden Code erstellen:

```Javascript
fs.writeFile('meineDatei.txt', 'Dies ist ein Beispieltext.', (err) => {
  if (err) throw err;
  console.log('Die Datei wurde erfolgreich erstellt!');
});
```

Der Code oben erstellt eine neue Datei mit dem Namen "meineDatei.txt" und fügt den Text "Dies ist ein Beispieltext." hinzu. Wenn die Datei bereits existiert, wird sie überschrieben. Die Funktion `writeFile` akzeptiert drei Argumente: der Name der Datei, der Inhalt der Datei und eine Callback-Funktion. Die Callback-Funktion wird ausgeführt, wenn die Datei erfolgreich erstellt wurde.

Um einer bestehenden Textdatei neuen Inhalt hinzuzufügen, können Sie die `appendFile` Funktion verwenden:

```Javascript
fs.appendFile('meineDatei.txt', '\nDies ist ein neuer Text.', (err) => {
  if (err) throw err;
  console.log('Der neue Text wurde erfolgreich hinzugefügt!');
});
```

Der Code oben fügt den Text "\nDies ist ein neuer Text." am Ende der Datei hinzu. Das "\n" wird benötigt, um einen Zeilenumbruch einzufügen.

Um den Inhalt einer Textdatei zu lesen, können Sie `readFile` verwenden:

```Javascript
fs.readFile('meineDatei.txt', 'utf8', (err, data) => {
  if (err) throw err;
  console.log(data);
});
```

Die `readFile` Funktion akzeptiert zwei Argumente: der Name der Datei und die Kodierung. In diesem Beispiel verwenden wir die Kodierung "utf8", um sicherzustellen, dass der Inhalt der Datei als Text und nicht als Binärdaten gelesen wird. Im Callback erhalten wir den Inhalt der Datei als `data` zurück, den wir dann in der Konsole ausgeben.

## Tiefer eintauchen

Wenn Sie mehr Kontrolle über die Erstellung von Textdateien mit JavaScript haben möchten, können Sie die `createWriteStream` Funktion verwenden. Hiermit können Sie den Inhalt und das Encoding der Datei angeben und auch mehrere Datenblöcke gleichzeitig schreiben. Ein Beispiel dafür wäre:

```Javascript
const writeStream = fs.createWriteStream('meineDatei.txt', 'utf8');
writeStream.write('Dies ist der erste Textblock.');
writeStream.write('Dies ist der zweite Textblock.');
writeStream.end();
```

Der Code oben erstellt ein schreibbares Stream-Objekt für die Datei "meineDatei.txt" mit dem Encoding "utf8". Mit der `write` Funktion können wir nun beliebig viele Textblöcke schreiben und mit `end` den Stream schließen.

## Siehe auch

- [Node.js Dokumentation - File System](https://nodejs.org/api/fs.html)
- [Tutorial: Node.js Dateien lesen, schreiben und erstellen](https://www.freecodecamp.org/news/node-js-tutorial-create-read-update-and-delete-files/)