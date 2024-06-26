---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:28:16.378032-07:00
description: "Wie: In einer Node.js-Umgebung k\xF6nnen Sie das integrierte `fs` (File\
  \ System) Modul verwenden, um Textdateien zu schreiben. Dieses Beispiel demonstriert\u2026"
lastmod: '2024-03-13T22:44:54.284746-06:00'
model: gpt-4-0125-preview
summary: "In einer Node.js-Umgebung k\xF6nnen Sie das integrierte `fs` (File System)\
  \ Modul verwenden, um Textdateien zu schreiben."
title: Eine Textdatei schreiben
weight: 24
---

## Wie:
In einer Node.js-Umgebung können Sie das integrierte `fs` (File System) Modul verwenden, um Textdateien zu schreiben. Dieses Beispiel demonstriert das asynchrone Schreiben von Text in eine Datei:

```javascript
const fs = require('fs');

const data = 'Hallo, Welt! Dies ist Text, der in eine Datei geschrieben werden soll.';

fs.writeFile('example.txt', data, (err) => {
  if (err) {
    throw err;
  }
  console.log('Datei wurde geschrieben.');
});
```

Beispielausgabe:
```
Datei wurde geschrieben.
```

Für synchrones Schreiben von Dateien verwenden Sie `writeFileSync`:
```javascript
try {
  fs.writeFileSync('example.txt', data);
  console.log('Datei wurde geschrieben.');
} catch (error) {
  console.error('Fehler beim Schreiben der Datei:', error);
}
```

In modernen Webbrowsern führt die File System Access API die Fähigkeit ein, Dateien zu lesen und zu schreiben. Allerdings ist deren Nutzung Nutzerberechtigungen unterworfen. So erstellen und schreiben Sie eine Datei:

```javascript
if ('showSaveFilePicker' in window) {
  const handle = await window.showSaveFilePicker();
  const writable = await handle.createWritable();
  await writable.write('Hallo, Welt! Dies ist das Schreiben einer Textdatei im Browser.');
  await writable.close();
}
```

Für komplexere Szenarien oder beim Arbeiten mit großen Dateien könnten Sie sich für Drittanbieter-Bibliotheken wie `FileSaver.js` für Browser entscheiden:

```html
<script src="https://cdnjs.cloudflare.com/ajax/libs/FileSaver.js/2.0.2/FileSaver.min.js"></script>
<script>
  const blob = new Blob(["Hallo, Welt! Dies ist Text von FileSaver.js."], {type: "text/plain;charset=utf-8"});
  saveAs(blob, "example.txt");
</script>
```

Denken Sie daran, dass das Schreiben von Dateien auf der Client-Seite (in Browsern) aufgrund von Sicherheitsbedenken eingeschränkt ist und jede Operation, die ein Speichern auf der lokalen Festplatte des Benutzers erfordert, in der Regel deren ausdrückliche Erlaubnis benötigt.
