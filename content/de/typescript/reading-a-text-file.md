---
title:                "TypeScript: Ein Textdokument lesen"
simple_title:         "Ein Textdokument lesen"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/typescript/reading-a-text-file.md"
---

{{< edit_this_page >}}

#Warum

Das Lesen von Textdateien ist eine grundlegende Fähigkeit, die für das Programmieren unerlässlich ist. In dieser Anleitung zeigen wir, wie Sie mithilfe von TypeScript Textdateien lesen können.

##So geht's

Um eine Textdatei in TypeScript zu lesen, müssen wir zunächst die eingebaute Node.js-Modul "fs" importieren. Dieses Modul bietet uns die Möglichkeit, auf das Dateisystem des Computers zuzugreifen. Wir können dann die Methode "readFileSync" verwenden, um die Textdatei zu lesen und in eine Variable zu speichern.

```TypeScript
import * as fs from 'fs';

let text = fs.readFileSync('datei.txt', 'utf-8');
```

In diesem Beispiel haben wir die Datei mit dem Namen "datei.txt" gelesen und den Inhalt in der Variablen "text" gespeichert. Der zweite Parameter "utf-8" stellt sicher, dass der Inhalt der Datei als Text und nicht als Binärdaten gelesen wird.

Um den Inhalt der Datei zu überprüfen, können wir einfach die Variable "text" ausgeben.

```TypeScript
console.log(text);
```

Die Ausgabe wird den gesamten Inhalt der Datei in der Konsole anzeigen.

##Tiefgehende Analyse

Beim Lesen von Textdateien gibt es einige wichtige Dinge zu beachten. Zum Beispiel müssen wir sicherstellen, dass die Datei vorhanden ist, bevor wir versuchen, sie zu lesen. Wir können dies mit der "existsSync" Methode überprüfen.

```TypeScript
if (fs.existsSync('datei.txt')) {
  let text = fs.readFileSync('datei.txt', 'utf-8');
  console.log(text);
} else {
  console.log('Datei nicht gefunden.');
}
```

Außerdem sollten wir uns bewusst sein, dass das Lesen von Textdateien synchron erfolgt, was bedeutet, dass das Programm pausieren wird, während die Datei eingelesen wird. Für große Dateien kann dies zu Leistungsproblemen führen. Eine bessere Alternative wäre das Lesen von Dateien asynchron mit der "readFile" Methode.

##Siehe auch

- [Node.js fs Modul Dokumentation](https://nodejs.org/api/fs.html)
- [TypeStrong/ts-node](https://github.com/TypeStrong/ts-node)
- [TypeScript offizielle Webseite](https://www.typescriptlang.org/)