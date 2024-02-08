---
title:                "Arbeiten mit JSON"
aliases:
- de/javascript/working-with-json.md
date:                  2024-02-03T19:22:59.624858-07:00
model:                 gpt-4-0125-preview
simple_title:         "Arbeiten mit JSON"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/javascript/working-with-json.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Was & Warum?

JSON (JavaScript Object Notation) ist ein leichtgewichtiges Daten-Austauschformat, einfach für Menschen zu lesen und zu schreiben sowie für Maschinen zu parsen und zu erzeugen. Programmierer nutzen es, um Daten in Web-Applikationen zu speichern und zu transportieren, wodurch es das Rückgrat der modernen API- und Web-Services-Kommunikation bildet.

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
