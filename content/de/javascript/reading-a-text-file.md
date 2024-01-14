---
title:    "Javascript: Eine Textdatei lesen"
keywords: ["Javascript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/javascript/reading-a-text-file.md"
---

{{< edit_this_page >}}

# Warum

Das Lesen einer Textdatei ist ein grundlegendes Konzept in der Programmierung und ein wichtiger Schritt bei der Verarbeitung von Daten. Es ermöglicht uns, Informationen aus einer Datei zu extrahieren und in unserem Code zu manipulieren.

# Wie Geht Das

In Javascript gibt es verschiedene Methoden, um eine Textdatei zu lesen. Eine Möglichkeit ist die Verwendung der `readFileSync()` Funktion aus dem `fs` Modul.

In unserem Beispiel haben wir eine Datei namens "liste.txt" mit einer Liste von Zahlen, jeweils in einer neuen Zeile. Unser Ziel ist es, diese Zahlen in ein Array zu speichern und sie dann in der Konsole auszugeben.

```Javascript
const fs = require('fs');

// Lese die Textdatei und speichere den Inhalt in einer Variable
const text = fs.readFileSync('liste.txt', 'utf-8');

// Trenne den Inhalt an jeder neuen Zeile und speichere es in einem Array
const array = text.split('\n');

// Gib jedes Element des Arrays in der Konsole aus
array.forEach(element => {
    console.log(element);
});

// Ausgabe:
// 1
// 2
// 3
// 4
// 5
```

# Tief Tauchen

Neben der `readFileSync()` Funktion gibt es noch andere Möglichkeiten, eine Textdatei zu lesen, wie z.B. die Verwendung von `readFile()` oder `createReadStream()` Funktionen. Diese bieten zusätzliche Funktionen und Flexibilität beim Lesen von Dateien.

Es ist auch wichtig zu beachten, dass beim Lesen einer Datei möglicherweise Fehler auftreten können. Deshalb ist es ratsam, immer eine Fehlerbehandlung in den Code einzubauen, um sicherzustellen, dass unser Programm ordnungsgemäß ausgeführt wird.

# Siehe Auch

- [Node.js Dokumentation - File System Modul](https://nodejs.org/api/fs.html)
- [W3Schools - Reading files with Node.js](https://www.w3schools.com/nodejs/nodejs_filesystem.asp)
- [Tutorialspoint - Node.js File System](https://www.tutorialspoint.com/nodejs/nodejs_file_system.htm)