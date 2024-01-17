---
title:                "Lesen einer Textdatei"
html_title:           "TypeScript: Lesen einer Textdatei"
simple_title:         "Lesen einer Textdatei"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/typescript/reading-a-text-file.md"
---

{{< edit_this_page >}}

Was & Warum?:
Beim Lesen einer Textdatei geht es darum, den Inhalt einer Textdatei in einem Computerprogramm zu verarbeiten. Programmierer tun dies, um auf die Informationen in der Datei zuzugreifen und sie in ihrem Programm zu nutzen.

Wie geht's:
Um eine Textdatei in TypeScript zu lesen, können wir die ```fs``` Bibliothek verwenden. Wir importieren sie mit ```import fs from 'fs'``` und verwenden dann die ```readFileSync()``` Methode, um die Datei zu lesen und ihren Inhalt in einer Variable zu speichern. Hier ist ein Beispiel:

 ```
import fs from 'fs';
let fileContent = fs.readFileSync('textdatei.txt', 'utf-8');
console.log(fileContent);
```

Dieser Code liest die Datei ```textdatei.txt``` und speichert ihren Inhalt in der Variablen ```fileContent```. Dann geben wir den Inhalt mit ```console.log()``` aus.

Tieferes Eintauchen:
Die Möglichkeit, Textdateien zu lesen, ist ein wichtiger Aspekt in der Programmierung und wird seit den Anfängen von Programmiersprachen wie C und Java verwendet. Alternativen zu der oben genannten Methode sind die Verwendung von ```readFile()```, einer asynchronen Methode, oder das Parsen der Datei mit einer regulären Ausdrucksbibliothek.

Außerdem ist es wichtig zu beachten, dass beim Lesen von Textdateien in TypeScript die Codierung angegeben werden muss. In unserem Beispiel haben wir ```utf-8``` als Codierung verwendet, aber dies kann je nach Datei variieren. Um dies zu vermeiden, können wir ```readFile()``` verwenden, um eine sogenannte Byte Buffer zurückzugeben, die es uns erlaubt, den Inhalt in der gewünschten Codierung zu verarbeiten.

Weitere Infos:
- Dokumentation der ```fs``` Bibliothek: https://nodejs.org/api/fs.html
- Tutorial zum Lesen von Dateien in TypeScript: https://www.tutorialspoint.com/typescript/typescript_reading_a_file.htm

Sieh auch:
- Vergleich von ```readFileSync()``` und ```readFile()```: https://stackoverflow.com/questions/17699599/node-js-quick-file-read
- Nützliche Tipps für den Umgang mit Dateien in TypeScript: https://itnext.io/working-with-files-in-typescript-part-1-reading-files-9ee2975a8e49