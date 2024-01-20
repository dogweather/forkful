---
title:                "Eine Textdatei lesen"
html_title:           "Bash: Eine Textdatei lesen"
simple_title:         "Eine Textdatei lesen"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/javascript/reading-a-text-file.md"
---

{{< edit_this_page >}}

# Lesen einer Textdatei in JavaScript: Eine kurze und unkomplizierte Anleitung

## Was & Warum?
Eine Textdatei lesen bedeutet, den gesamten oder einen Teil des Inhalts einer Textdatei mit einer bestimmten Programmiersprache zu erfassen. Als Programmierer tun wir dies, um Daten zu analysieren, zu manipulieren und zu verwenden.

## So geht's:
Es gibt verschiedene Möglichkeiten, eine Textdatei in JavaScript zu lesen. Die moderne Art ist die Verwendung der File-API, die Damit können wir Dateien lesen, die vom Nutzer hochgeladen wurden.

```Javascript
let input = document.querySelector('input[type="file"]');

input.addEventListener('change', function() {
  let reader = new FileReader();

  reader.addEventListener('load', function() {
    console.log(reader.result);
  });

  reader.readAsText(input.files[0]);
});
```

Wenn Sie diesen Code ausführen und eine Textdatei hochladen, werden Sie den Inhalt der Datei in der Konsole sehen.

## Vertiefung
Historisch gesehen, lasen wir Textdateien auf serverseitigem JavaScript mithilfe des `fs` Moduls von Node.js. Aber jetzt preferieren wir die File-API für seine Einfachheit und Effizienz.

```Javascript
const fs = require('fs');

fs.readFile('/Pfad/zur/Datei.txt', 'utf8', function(err, data) {
  if (err) throw err;
  console.log(data);
});
```

Die File-API hat jedoch Nachteile: Sie kann nur bei Dateien verwendet werden, die vom Benutzer hochgeladen wurden. Für serverseitige Aktionen bleibt `fs` die Hauptoption.

## Siehe auch
Um mehr über die File-API zu erfahren, besuchen Sie die [MDN-Webseite](https://developer.mozilla.org/de/docs/Web/API/File/Using_files_from_web_applications).
Weitere Informationen zum `fs` Modul finden Sie in der [Node.js-Dokumentation](https://nodejs.org/api/fs.html).

Verwenden Sie die richtigen Werkzeuge für Ihre spezifischen Anforderungen. Vergessen Sie nicht, dass Lernen durch Üben kommt. Frohes Codieren!