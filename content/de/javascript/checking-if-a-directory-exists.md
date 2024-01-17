---
title:                "Überprüfen, ob ein Verzeichnis vorhanden ist"
html_title:           "Javascript: Überprüfen, ob ein Verzeichnis vorhanden ist"
simple_title:         "Überprüfen, ob ein Verzeichnis vorhanden ist"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/javascript/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Was & Warum?
Das Überprüfen, ob ein Verzeichnis existiert, ist eine Möglichkeit für Programmierer, sicherzustellen, dass ein bestimmter Speicherort auf ihrem Computer vorhanden ist, bevor sie darauf zugreifen oder damit arbeiten. Dies ist besonders nützlich, um unerwartete Fehler zu vermeiden, die auftreten können, wenn ein Programm versucht, auf ein nicht vorhandenes Verzeichnis zuzugreifen.

## Wie geht das?
Es gibt mehrere Möglichkeiten, in Javascript zu überprüfen, ob ein Verzeichnis existiert. Eine Möglichkeit ist die Verwendung der `fs.existsSync()`-Funktion aus dem File System Modul. Ein Beispielcode könnte wie folgt aussehen:

```Javascript
const fs = require('fs');

if (fs.existsSync('./verzeichnis')) {
  console.log('Verzeichnis existiert!');
} else {
  console.log('Verzeichnis existiert nicht!');
}
```

Das Programm prüft, ob das Verzeichnis `verzeichnis` im aktuellen Verzeichnis vorhanden ist und gibt entsprechend eine Meldung aus.

## Tiefere Einblicke
Das Überprüfen von Verzeichnissen hat seinen Ursprung in der Notwendigkeit, in älteren Programmiersprachen wie C oder C++ bestimmte Dateien für ein Programm zu finden. Alternativ zur `fs.existsSync()`-Funktion kann auch die `fs.accessSync()`-Funktion verwendet werden, die detailliertere Informationen über die Berechtigungen des Verzeichnisses liefert. Eine weitere Alternative wäre die Verwendung von regelmäßigen Ausdrücken für eine komplexere Überprüfung.

## Siehe dazu
- [Node.js Dokumentation zur `fs`-Modul](https://nodejs.org/api/fs.html)
- [Tutorial zu Node.js und Verzeichnisüberprüfungen](https://www.geeksforgeeks.org/node-js-fs-existsync-method/)