---
title:                "Prüfen, ob ein Verzeichnis existiert"
html_title:           "Javascript: Prüfen, ob ein Verzeichnis existiert"
simple_title:         "Prüfen, ob ein Verzeichnis existiert"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/javascript/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

# Prüfen, ob ein Verzeichnis in JavaScript existiert

## Was & Warum?
Die Überprüfung, ob ein Verzeichnis in JavaScript existiert, ist ein grundlegender aber wichtiger Aspekt des Dateisystems. Es wird durchgeführt, um Fehler zu vermeiden, bevor Operationen auf Dateien und Verzeichnisse angewendet werden.

## Wie wird es gemacht:

```Javascript
const fs = require('fs');

function dirExists(dirPath) {
  try {
    return fs.statSync(dirPath).isDirectory();
  } catch (err) {
    return false;
  }
}

console.log(dirExists('./your-directory'));  // Ausgabe: true oder false
```

In obigem Code verwenden wir das `fs` (File System) Modul in Node.js, um auf das Dateisystem des Computers zuzugreifen. Wir prüfen, ob das Verzeichnis mit dem gegebenen Pfad existiert und ein Verzeichnis ist.

## Tiefgreifende Infos

1. Historischer Kontext: Frühere Versionen von Node.js verwendeten `fs.existsSync()`, das mittlerweile als veraltet gilt. Es wird empfohlen, `fs.statSync().isDirectory()` zu verwenden, da dies Fehler wirft und behandelt, und so eine robustere Anwendung ermöglicht.

2. Alternativen: Es gibt verschiedene NPM-Pakete wie `fs-extra`, die ähnliche Funktionen bieten und möglicherweise zusätzliche Funktionen bereitstellen oder der bevorzugten Programmierstilrichtung entsprechen.

3. Umsetzungsdetails: Nicht vorhandene Dateien und Verzeichnisse in Node.js heben eine Ausnahme hervor. Daher verwenden wir `try...catch` um diese Ausnahme abzufangen, ohne das Programm zu beenden.

## Siehe auch  

* [Node.js `fs` Modul Dokumentation](https://nodejs.org/api/fs.html)  
* [`fs-extra` auf npm](https://www.npmjs.com/package/fs-extra)  