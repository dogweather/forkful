---
title:                "Arbeiten mit JSON"
date:                  2024-01-19
html_title:           "Arduino: Arbeiten mit JSON"
simple_title:         "Arbeiten mit JSON"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/typescript/working-with-json.md"
---

{{< edit_this_page >}}

## What & Why?
JSON (JavaScript Object Notation) ist ein kompaktes Datenformat, das leicht von Menschen gelesen und von Maschinen verarbeitet werden kann. Programmierer nutzen es wegen seiner Einfachheit und weil es Sprachübergreifend für die Datenkommunikation, besonders in Webanwendungen, verwendet wird.

## How to:
Mit TypeScript ist das Arbeiten mit JSON einfach. Hier sind grundlegende Beispiele.

### Ein JSON-Objekt definieren:
```TypeScript
let user: any = '{"name": "Max", "age": 25}';
```

### JSON in ein TypeScript Objekt umwandeln:
```TypeScript
let userObj: { name: string; age: number } = JSON.parse(user);
console.log(userObj); // Ausgabe: { name: 'Max', age: 25 }
```

### Ein TypeScript Objekt in einen JSON-String umwandeln:
```TypeScript
let jsonString: string = JSON.stringify(userObj);
console.log(jsonString); // Ausgabe: '{"name":"Max","age":25}'
```

## Deep Dive
Die Anfänge von JSON reichen in die frühen 2000er zurück, wo es als vereinfachtes Format für die Datenübertragung in JavaScript entwickelt wurde. Mittlerweile wird es in vielen Programmiersprachen unterstützt und gilt als Standard für APIs, also Web-Schnittstellen. Alternativen wie XML sind weniger beliebt, da sie umständlicher zu lesen und zu schreiben sind. In TypeScript sind `JSON.parse()` und `JSON.stringify()` wichtige Methoden, um mit JSON zu arbeiten und Objekte zwischen String-Darstellung und nutzbarem Code zu konvertieren.

## See Also
Weitere Informationen und detaillierte Beispiele findest du in der offiziellen TypeScript Dokumentation:
- JSON.org: [Introduction to JSON](https://json.org/json-de.html)
- MDN Web Docs: [Arbeiten mit JSON](https://developer.mozilla.org/de/docs/Learn/JavaScript/Objects/JSON)
