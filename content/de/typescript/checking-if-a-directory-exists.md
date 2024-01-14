---
title:    "TypeScript: Überprüfen, ob ein Verzeichnis existiert"
keywords: ["TypeScript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/typescript/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Warum

Oftmals müssen ProgrammiererInnen überprüfen, ob ein bestimmtes Verzeichnis vorhanden ist, bevor sie weitere Aktionen durchführen können. Dies kann zum Beispiel notwendig sein, um sicherzustellen, dass eine Datei in einem bestimmten Ordner existiert, bevor sie gelesen oder bearbeitet wird. In diesem Blogbeitrag zeigen wir, wie man mithilfe von TypeScript überprüfen kann, ob ein Verzeichnis existiert.

## How To

Um zu überprüfen, ob ein Verzeichnis existiert, kann die Funktion `existsSync()` aus dem Modul `fs` verwendet werden. Diese Funktion gibt einen booleschen Wert zurück, der angibt, ob das angegebene Verzeichnis existiert oder nicht. Im folgenden Beispiel überprüfen wir, ob das Verzeichnis "projekt" existiert:

```TypeScript
import * as fs from 'fs';

if(fs.existsSync('projekt')) {
  console.log('Das Verzeichnis existiert.');
} else {
  console.log('Das Verzeichnis existiert nicht.');
}
```

Wenn das Verzeichnis existiert, wird die erste Ausgabe angezeigt, ansonsten die zweite.

## Deep Dive

Ein tieferes Verständnis über die Funktionsweise von `existsSync()` kann erreicht werden, indem wir uns den zugrundeliegenden Code ansehen. Diese Funktion nutzt den `stat` Befehl des Betriebssystems, um Informationen über ein bestimmtes Verzeichnis zu erhalten. Diese Informationen werden dann genutzt, um den booleschen Wert zurückzugeben. Wenn das Verzeichnis nicht gefunden werden kann, wird ein Fehler ausgelöst.

Es gibt auch die Möglichkeit, asynchrone Versionen dieser Funktion zu nutzen, wie `exists()` oder `stat()`, die mit Promises oder `await` verwendet werden können.

## Siehe auch

- [Node.js fs Modul Dokumentation](https://nodejs.org/dist/latest-v14.x/docs/api/fs.html)
- [Tutorial: Using the Node.js fs Module](https://www.digitalocean.com/community/tutorials/nodejs-reading-files#using-node-js-fs-module)
- [Richtig asynchrone Programmierung mit TypeScript und async/await](https://t3n.de/news/asynchron-programmieren-typescript-628810/)