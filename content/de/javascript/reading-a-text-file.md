---
title:                "Javascript: Eine Textdatei lesen"
simple_title:         "Eine Textdatei lesen"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/javascript/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Warum
Wenn man mit Javascript programmieren möchte, kann es oft nützlich sein, Textdateien zu lesen. Das ermöglicht es, Daten aus externen Dateien in das Programm einzubinden und weiterzuverarbeiten. In diesem Artikel werden wir uns anschauen, wie man genau das macht und welche Methoden dabei zur Verfügung stehen.

## Wie man eine Textdatei liest
Um eine Textdatei in Javascript zu lesen, müssen wir zunächst eine Verbindung zu der Datei herstellen. Dafür nutzen wir die `fs` Bibliothek, die bereits in Node.js integriert ist. Um diese zu nutzen, müssen wir sie zuerst importieren:

```Javascript
const fs = require('fs');
```

Als nächstes müssen wir den Pfad zur Textdatei angeben, die wir lesen möchten. Angenommen, die Datei befindet sich im selben Ordner wie unser Skript, können wir einfach den Dateinamen angeben. Andernfalls müssen wir den Pfad zur Datei angeben, zum Beispiel `./Ordner/datei.txt`.

```Javascript
const path = 'datei.txt';
```

Jetzt können wir die Methode `readFileSync()` aufrufen, die uns die Datei im Form einer Bytefolge zurückgibt. Um diesen Datenstream in einen lesbaren String umzuwandeln, können wir die Methode `toString()` verwenden:

```Javascript
const data = fs.readFileSync(path).toString();
console.log(data);
```

Dies gibt den gesamten Inhalt der Datei in der Konsole aus. Um die Daten weiter zu verarbeiten, können wir sie in ein Array aufteilen, indem wir die `split()` Methode nutzen und das Trennzeichen angeben. Angenommen, die Datei besteht aus mehreren Zeilen, können wir diese so in ein Array aufteilen:

```Javascript
const lines = data.split('\n');
console.log(lines);
```

Dies gibt ein Array zurück, wobei jedes Element eine Zeile aus der Textdatei darstellt. Nun können wir mit den Daten weiterarbeiten und zum Beispiel bestimmte Werte auslesen oder weiterverarbeiten.

## Tieferer Einstieg
Das waren nur die Grundlagen für das Lesen einer Textdatei in Javascript. Es gibt auch noch die Möglichkeit, asynchron auf die Datei zuzugreifen, indem man die Methode `readFile()` nutzt und eine Callback-Funktion verwendet. Außerdem gibt es verschiedene Optionen, um die Datei in verschiedenen Codierungen zu lesen oder direkt als JSON-Objekt einzulesen. Hier lohnt es sich, weiter in die Dokumentation der `fs` Bibliothek einzusteigen.

## Siehe auch
- [Node.js fs Module Dokumentation](https://nodejs.org/api/fs.html)
- [Codebeispiel für das Lesen einer Textdatei in Javascript](https://www.w3schools.com/nodejs/nodejs_filesystem.asp)
- [Eine tiefergehende Erklärung zu Callback-Funktionen](https://developer.mozilla.org/en-US/docs/Glossary/Callback_function)