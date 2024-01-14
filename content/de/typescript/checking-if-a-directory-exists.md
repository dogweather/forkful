---
title:                "TypeScript: Überprüfen, ob ein Verzeichnis vorhanden ist"
programming_language: "TypeScript"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/typescript/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Warum

In vielen TypeScript-Projekten müssen wir überprüfen, ob ein bestimmter Ordner bereits vorhanden ist, bevor wir ihn manipulieren oder auf seine Inhalte zugreifen können. Dies ist besonders wichtig, wenn wir mit externen Dateien arbeiten oder Anwendungen entwickeln, die auf unterschiedlichen Umgebungen ausgeführt werden können. Die Überprüfung auf die Existenz eines Verzeichnisses ist daher ein wichtiger Schritt, um sicherzustellen, dass unser Code fehlerfrei und reibungslos funktioniert.

## Wie

Um zu überprüfen, ob ein Verzeichnis vorhanden ist, können wir die `fs`-Bibliothek von Node.js verwenden. Zunächst müssen wir sie in unserem TypeScript-Code importieren:

```TypeScript
import * as fs from 'fs';
```

Dann können wir die Methode `existsSync()` verwenden, um die Existenz eines Verzeichnisses zu überprüfen. Diese Methode erwartet den Pfad des Verzeichnisses als Argument und gibt `true` zurück, wenn es vorhanden ist, oder `false` wenn nicht.

Ein Beispiel könnte so aussehen:

```TypeScript
if (fs.existsSync('/Users/benutzer/dokumente')) {
    console.log('Das Verzeichnis existiert');
} else {
    console.log('Das Verzeichnis existiert nicht');
}
```

Die Ausgabe dieses Codes auf meinem System wäre `Das Verzeichnis existiert`, da ich dieses Verzeichnis auf meinem Computer habe.

## Deep Dive

Die `existsSync()`-Methode synchron überprüft die Existenz des angegebenen Verzeichnisses. Das bedeutet, dass die Ausführung des Codes blockiert wird, bis das Ergebnis zurückgegeben wird. Für asynchrone Überprüfungen, die die Ausführung des Codes nicht blockieren, können wir die Methode `exists()` verwenden, die eine Rückruffunktion erwartet.

Zum Beispiel:

```TypeScript
fs.exists('/Users/benutzer/dokumente', (exists) => {
    if (exists) {
        console.log('Das Verzeichnis existiert');
    } else {
        console.log('Das Verzeichnis existiert nicht');
    }
});
```

Beachten Sie jedoch, dass die Verwendung von `exists()` in TypeScript möglicherweise zu einem Fehler führt, da es nicht die vollständige Typisierung für die Rückruffunktion aufweist. Es wird empfohlen, stattdessen `existsSync()` zu verwenden.

## Siehe auch

- Node.js `fs`-Dokumentation: https://nodejs.org/api/fs.html
- Weitere Dateioperationen in TypeScript: https://www.typescriptlang.org/docs/handbook/file-system-namespace.html