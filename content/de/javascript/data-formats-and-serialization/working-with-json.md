---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:22:59.624858-07:00
description: 'Wie geht das: #.'
lastmod: '2024-03-13T22:44:54.287714-06:00'
model: gpt-4-0125-preview
summary: '#.'
title: Arbeiten mit JSON
weight: 38
---

## Wie geht das:


### Parsen von JSON
Um einen JSON-String in ein JavaScript-Objekt umzuwandeln, verwenden Sie `JSON.parse()`.

```javascript
const jsonString = '{"name":"John", "age":30, "city":"New York"}';
const obj = JSON.parse(jsonString);
console.log(obj.name); // Ausgabe: John
```

### JavaScript-Objekte in Strings umwandeln
Um ein JavaScript-Objekt zurück in einen JSON-String umzuwandeln, verwenden Sie `JSON.stringify()`.

```javascript
const user = { name: "Jane", age: 25, city: "London" };
const jsonString = JSON.stringify(user);
console.log(jsonString); // Ausgabe: {"name":"Jane","age":25,"city":"London"}
```

### Umgang mit Dateien in Node.js
Um eine JSON-Datei zu lesen und sie in ein Objekt in einer Node.js-Umgebung umzuwandeln, können Sie das `fs`-Modul verwenden. Dieses Beispiel setzt voraus, dass Sie eine Datei namens `data.json` haben.

```javascript
const fs = require('fs');

fs.readFile('data.json', 'utf-8', (err, data) => {
    if (err) werfen Sie err;
    const obj = JSON.parse(data);
    console.log(obj);
});
```

Um ein Objekt in eine JSON-Datei zu schreiben:

```javascript
const fs = require('fs');
const user = { name: "Mike", age: 22, city: "Berlin" };

fs.writeFile('user.json', JSON.stringify(user, null, 2), (err) => {
    if (err) werfen Sie err;
    console.log('Daten in Datei geschrieben');
});
```

### Drittanbieter-Bibliotheken
Für komplexe JSON-Operationen können Frameworks und Bibliotheken wie `lodash` die Aufgaben vereinfachen, doch für grundlegende Operationen sind oft die einheimischen JavaScript-Funktionen ausreichend. Für groß angelegte oder leistungskritische Anwendungen können Sie Bibliotheken wie `fast-json-stringify` für eine schnellere JSON-Stringifizierung oder `json5` für das Parsen und Stringifizieren unter Verwendung eines flexibleren JSON-Formats in Betracht ziehen.

Parsen mit `json5`:
```javascript
const JSON5 = require('json5');

const jsonString = '{name:"John", age:30, city:"New York"}';
const obj = JSON5.parse(jsonString);
console.log(obj.name); // Ausgabe: John
```

Diese Beispiele decken grundlegende Operationen mit JSON in JavaScript ab, perfekt geeignet für Anfänger, die von anderen Sprachen wechseln und effizient Daten in Webanwendungen handhaben möchten.
