---
title:                "Eine Textdatei lesen."
html_title:           "Javascript: Eine Textdatei lesen."
simple_title:         "Eine Textdatei lesen."
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/javascript/reading-a-text-file.md"
---

{{< edit_this_page >}}

Was ist Lesen einer Textdatei und warum machen Programmierer es?

Das Lesen einer Textdatei ist das Einlesen von Inhalten aus einer Textdatei in einen Computer oder ein Programm. Programmierer tun dies, um auf die in der Textdatei enthaltenen Daten zuzugreifen und diese in ihrem Code zu verwenden.

Wie geht das?

Das Lesen einer Textdatei kann in Javascript auf verschiedene Arten erfolgen. Eine Möglichkeit ist die Verwendung der "fs" -Bibliothek, die Funktionen zum Lesen von Dateien bereitstellt. Hier ist ein Beispielcode, der den Inhalt einer Datei namens "text.txt" ausgibt:

```Javascript
const fs = require('fs');
fs.readFile('text.txt', 'utf-8', (err, data) => {
  if (err) throw err;
  console.log(data);
});
```

Die Ausgabe wird den Inhalt der "text.txt" -Datei in der Konsole anzeigen.

Tieferer Einblick

Das Lesen von Textdateien ist eine wichtige Funktion für Programmierer, da sie es ermöglicht, auf externe Daten zuzugreifen und diese in ihren Code zu integrieren. In der Vergangenheit mussten Programmierer komplexe Algorithmen erstellen, um Textdateien zu lesen, aber mit der Einführung von Bibliotheken wie "fs" ist das Lesen von Textdateien wesentlich einfacher geworden. Es ist wichtig zu beachten, dass das Lesen von Textdateien nicht die einzige Möglichkeit ist, auf externe Daten zuzugreifen. Es gibt auch andere Methoden wie das Lesen von JSON-Dateien oder das Anfordern von Daten von einer API.

Siehe auch

Weitere Informationen zum Lesen von Textdateien in Javascript finden Sie in der offiziellen Dokumentation von Node.js unter: https://nodejs.org/api/fs.html#fs_fs_readfile_path_options_callback