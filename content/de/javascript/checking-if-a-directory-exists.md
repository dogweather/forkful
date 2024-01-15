---
title:                "Überprüfen, ob ein Verzeichnis vorhanden ist."
html_title:           "Javascript: Überprüfen, ob ein Verzeichnis vorhanden ist."
simple_title:         "Überprüfen, ob ein Verzeichnis vorhanden ist."
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/javascript/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Warum
Warum sollte man überhaupt prüfen, ob ein Verzeichnis existiert? Nun, es gibt viele mögliche Szenarien, in denen es nützlich sein kann, dies zu tun. Zum Beispiel, wenn man sicherstellen möchte, dass ein bestimmter Ordner existiert, bevor man eine Operation ausführt, die darauf zugreift. Oder auch wenn man dynamische Pfade verwendet und sicherstellen möchte, dass ein bestimmtes Verzeichnis auf dem System vorhanden ist, bevor man darauf zugreift.

## Wie
Um zu überprüfen, ob ein Verzeichnis existiert, gibt es in Javascript verschiedene Möglichkeiten. Eine davon ist die Verwendung des `fs`-Moduls aus der Node.js-Bibliothek. Hier ist ein Beispielcode, der zeigt, wie man diese Methode nutzen kann:

```Javascript
const fs = require('fs');
const directory = '/Users/Username/Desktop/';

if (fs.existsSync(directory)) {
  console.log('Das Verzeichnis existiert!');
} else {
  console.log('Das Verzeichnis existiert nicht!');
}
```

In diesem Beispiel verwenden wir die `existsSync()`-Methode des `fs`-Moduls, die als Argument den Pfad des zu überprüfenden Verzeichnisses annimmt. Diese Methode gibt entweder `true` oder `false` zurück, je nachdem, ob das Verzeichnis existiert oder nicht. Im obigen Codebeispiel wird eine entsprechende Meldung auf der Konsole ausgegeben, basierend auf dem Rückgabewert der `existsSync()`-Methode.

Alternativ kann man auch die `statSync()`-Methode des `fs`-Moduls verwenden, um Informationen über ein Verzeichnis zu erhalten. Diese Methode gibt ein Objekt zurück, das verschiedene Eigenschaften des Verzeichnisses enthält, einschließlich `isDirectory()`, die prüft, ob es sich um ein Verzeichnis handelt. Hier ist ein Beispielcode, der diese Methode nutzt:

```Javascript
const fs = require('fs');
const directory = '/Users/Username/Desktop/';

const stats = fs.statSync(directory);

if (stats.isDirectory()) {
  console.log('Es handelt sich um ein Verzeichnis!');
} else {
  console.log('Es handelt sich nicht um ein Verzeichnis!');
}
```

## Deep Dive
Es ist wichtig zu beachten, dass die `fs`-Module von Node.js nur auf dem Server ausgeführt werden können, da sie auf das Dateisystem zugreifen. Für eine clientseitige Anwendung, die im Browser ausgeführt wird, gibt es keine direkte Möglichkeit, auf das Dateisystem zuzugreifen. In solchen Fällen kann man jedoch Dienste wie `electron` verwenden, um eine Anwendung zu erstellen, die auf dem Desktop läuft und Zugriff auf das Dateisystem hat.

Darüber hinaus gibt es auch Module von Drittanbietern, die diese Funktionalität in einem Browser bereitstellen, wie z.B. `browser-fs-access`. Es ist jedoch wichtig zu beachten, dass diese nicht offiziell unterstützt werden und möglicherweise nicht in allen Browsern funktionieren.

## Siehe auch
- Dokumentation zum `fs`-Modul in der Node.js-Bibliothek: https://nodejs.org/api/fs.html#fs_fs_exists_path_callback
- Beispiele für die Verwendung des `fs`-Moduls: https://www.geeksforgeeks.org/node-js-fs-exists-method/
- Dokumentation zum `electron`-Framework: https://www.electronjs.org/
- Informationen zum `browser-fs-access`-Modul: https://github.com/jvilk/BrowserFS