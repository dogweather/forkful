---
title:    "TypeScript: Einen Textdatei lesen"
keywords: ["TypeScript"]
---

{{< edit_this_page >}}

## Warum
Wenn du Textdateien in einem TypeScript-Programm lesen musst, gibt es verschiedene Gründe, warum du dich in diesem Artikel damit beschäftigen solltest. Du könntest zum Beispiel eine Konfigurationsdatei lesen, Benutzereingaben parsen oder Daten aus einer externen Quelle importieren. Lesen von Textdateien ist eine häufige Aufgabe in der Programmierung und in diesem Artikel erfährst du, wie du dies in TypeScript realisieren kannst.

## Wie es geht
Um eine Textdatei in TypeScript zu lesen, kannst du das `fs`-Modul verwenden, das bereits in Node.js integriert ist. Das folgende Beispiel zeigt, wie du eine Datei namens "beispiel.txt" im selben Verzeichnis wie dein TypeScript-Code liest und deren Inhalt auf der Konsole ausgibst:

```TypeScript
import fs from 'fs';

const datei = fs.readFileSync('beispiel.txt', 'utf-8');
console.log(datei);
```

Dieses Beispiel verwendet die `readFileSync`-Funktion des `fs`-Moduls, um synchron auf die Datei "beispiel.txt" zuzugreifen. Die erste Argument ist der Dateipfad und das zweite Argument gibt das Zeichenkodierungsschema an. In diesem Fall verwenden wir "utf-8", was für die meisten gängigen Textdateien funktioniert. Wenn die Datei erfolgreich gelesen wird, wird ihr Inhalt als Zeichenkette zurückgegeben und in der Konsole ausgegeben.

Es ist auch möglich, eine Datei asynchron mit dem `fs`-Modul zu lesen. Das folgende Beispiel zeigt, wie dies mit der `readFile`-Funktion gemacht werden kann:

```TypeScript
import fs from 'fs';

fs.readFile('beispiel.txt', 'utf-8', (error, datei) => {
  if (error) {
    console.error(`Fehler beim Lesen der Datei: ${error}`);
    return;
  }
  console.log(datei);
});
```

Die `readFile`-Funktion verwendet eine Callback-Funktion, die aufgerufen wird, wenn die Datei erfolgreich gelesen wurde oder ein Fehler aufgetreten ist. Das zweite Argument der Callback-Funktion gibt den Inhalt der Datei zurück, falls sie erfolgreich gelesen wurde. Ansonsten wird der Fehler ausgegeben.

## Tief einsteigen
Jetzt, da du weißt, wie du eine Textdatei in TypeScript lesen kannst, gibt es noch ein paar zusätzliche Dinge zu beachten. Zum einen solltest du immer überprüfen, ob die Datei existiert, bevor du versuchst, sie zu lesen. Dies kann mit der Funktion `existsSync` des `fs`-Moduls gemacht werden.

Ein weiterer wichtiger Punkt ist die Behandlung von Zeilenumbrüchen (Line Breaks). In Windows-Betriebssystemen wird normalerweise der Zeilenumbruch mit dem Zeichenkette "\r\n" dargestellt, während in Unix-Systemen nur "\n" verwendet wird. Um sicherzustellen, dass du die richtige Zeilenunterbrechung verwendest, kannst du die `os`-Module von Node.js und die `EOL`-Konstante verwenden.

```TypeScript
import os from 'os';

// EOL = End of Line = Zeilenumbruch
const zeilenunterbrechung = os.EOL;
```

## Siehe auch
- [Node.js `fs`-Modul Dokumentation](https://nodejs.org/api/fs.html)
- [Node.js `os`-Modul Dokumentation](https://nodejs.org/api/os.html)
- [EOL-Konstante in TypeScript](https://www.typescriptlang.org/docs/handbook/release-notes/typescript-3-9.html#newline-characters-in-strings-corrected)