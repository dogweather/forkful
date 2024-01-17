---
title:                "Das Schreiben einer Textdatei"
html_title:           "Javascript: Das Schreiben einer Textdatei"
simple_title:         "Das Schreiben einer Textdatei"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/javascript/writing-a-text-file.md"
---

{{< edit_this_page >}}

# Was & Warum?
Das Schreiben von Textdateien ist eine häufige Aufgabe für Programmierer, da es erlaubt, dauerhafte Daten zu speichern und leicht von anderen Programmen gelesen werden kann.

# Wie geht's?
```Javascript
// Hier ist ein Beispiel, wie man eine Textdatei erstellt und Text darin schreibt
const fs = require('fs'); // importieren des fs-Moduls

fs.writeFileSync('textdatei.txt', 'Hallo Welt!'); // Bilddaten in eine Textdatei schreiben

// Um eine Textdatei zu lesen, können wir die readFileSync-Funktion verwenden
const text = fs.readFileSync('textdatei.txt', 'utf8'); // speichern der Textdatei in der Variable 'text'

console.log(text); // Ausgabe des Inhalts der Textdatei (Hallo Welt!)
```

# Tiefere Einblicke
Textdateien sind eine der ältesten Arten von Datenformaten und werden immer noch häufig verwendet, um Informationen zu speichern, die einfach und direkt zugänglich sein müssen. Es gibt auch alternative Methoden, um Daten zu speichern, wie zum Beispiel Datenbanken oder Json-Dateien. Das Schreiben von Textdateien ist jedoch immer noch nützlich, wenn es um Einfachheit und Portabilität geht. Um eine Textdatei zu schreiben, müssen wir das fs-Modul in Node.js verwenden, das uns Zugriff auf das Dateisystem bietet. Durch die Verwendung von Funktionen wie writeFileSync können wir ganz einfach Text in eine Datei schreiben.

# Siehe auch
- [fs-Modul in der Node.js Dokumentation](https://nodejs.org/dist/latest-v14.x/docs/api/fs.html)
- [Ein Tutorial zum Schreiben von Textdateien in Javascript](https://www.w3schools.com/nodejs/nodejs_filesystem.asp)
- [Eine Einführung in das Schreiben von Dateien in Javascript](https://www.geeksforgeeks.org/how-to-write-a-file-synchronously-using-filesystems-in-node-js/)