---
title:    "TypeScript: Einen Textdatei lesen"
keywords: ["TypeScript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/typescript/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Warum
Textdateien sind eines der grundlegenden Dateiformate in der Programmierung. Durch das Lesen von Textdateien können wir wichtige Informationen wie Konfigurationsdaten oder Benutzereingaben verarbeiten. In diesem Blog-Beitrag werden wir uns ansehen, wie man Textdateien mit TypeScript liest.

## Wie man Textdateien mit TypeScript liest
Zunächst erstellen wir eine Beispieltextdatei mit dem Namen "beispiel.txt" mit folgendem Inhalt:

```
Dies ist eine Beispieltextdatei.
Wir verwenden sie, um das Lesen von Textdateien in TypeScript zu demonstrieren.
```

Nun können wir mit dem Lesen der Datei in unserer TypeScript-Datei beginnen:

```TypeScript
import fs from 'fs';

// Pfad zur Textdatei
const pfad = 'beispiel.txt';

// Asynchrones Lesen der Datei
fs.readFile(pfad, (fehler, daten) => {
    if (fehler) {
      // Fehlerbehandlung
      console.log(fehler);
    }
    // Konvertieren des Datentyps zu String
    const text = daten.toString();
    // Ausgabe des Dateiinhalts
    console.log(text);
});
```

Wenn wir diese TypeScript-Datei ausführen, erhalten wir als Ausgabe den Inhalt der Textdatei:

```
Dies ist eine Beispieltextdatei.
Wir verwenden sie, um das Lesen von Textdateien in TypeScript zu demonstrieren.
```

## Tiefer Einblick
Bei der Verwendung von `fs.readFile` müssen wir immer auf den asynchronen Rückgabewert warten, daher wird eine Callback-Funktion ausgeführt, sobald der Dateiinhalt verfügbar ist. In der Callback-Funktion können wir dann den Dateiinhalt als Binärdaten erhalten und müssen ihn noch in einen String konvertieren, um ihn lesbar zu machen.

Es gibt auch die Möglichkeit, Textdateien synchron mit `fs.readFileSync` zu lesen, aber dies blockiert den Ausführungsprozess, bis die Datei vollständig gelesen wurde. Daher ist es empfehlenswert, die asynchrone Methode zu verwenden.

## Siehe auch
- [Node.js Dokumentation zu fs.readFile](https://nodejs.org/api/fs.html#fs_fs_readfile_path_options_callback)
- [Tutorial zu TypeScript von der offiziellen Website](https://www.typescriptlang.org/docs/handbook/intro.html)
- [Weitere Beispiele zu fs.readFile mit TypeScript](https://www.digitalocean.com/community/tutorials/nodejs-reading-files-with-fsjs)