---
title:                "Arbeiten mit JSON"
html_title:           "Arduino: Arbeiten mit JSON"
simple_title:         "Arbeiten mit JSON"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/javascript/working-with-json.md"
---

{{< edit_this_page >}}

## Was & Warum?
JSON (JavaScript Object Notation) ist ein leichtgewichtiges Datenformat zum Austausch von Daten. Entwickler nutzen es wegen seiner Einfachheit und Weil's von so ziemlich jeder Programmiersprache gelesen werden kann.

## How to:
```Javascript
// JSON-String erstellen
let jsonString = '{"name": "Max", "alter": 25, "hobbys": ["Fahrradfahren", "Schwimmen"]}';

// JSON zu einem JavaScript-Objekt umwandeln
let objekt = JSON.parse(jsonString);

console.log(objekt.name); // Ausgabe: Max

// Ein JavaScript-Objekt zu einem JSON-String umwandeln
let neuerJsonString = JSON.stringify(objekt);

console.log(neuerJsonString); // Ausgabe: '{"name":"Max","alter":25,"hobbys":["Fahrradfahren","Schwimmen"]}'
```

## Deep Dive
JSON gibt's seit den frühen 2000ern, inspiriert von JavaScript, ist aber komplett unabhängig. XML war die Alternative, ist aber weniger effizient und schwerer zu lesen. Beim Umgang mit JSON geht's oft um `parse()` und `stringify()` in JavaScript. Auf Server-Seite ist's egal, ob du Node.js oder was anderes benutzt, JSON geht immer glatt durch.

## See Also
- [MDN Web Docs zum Thema JSON](https://developer.mozilla.org/de/docs/Web/JavaScript/Reference/Global_Objects/JSON)
- [JSON.org](https://www.json.org/json-de.html)
- [ECMAScript Spezifikation (gibt's auch auf Deutsch)](https://www.ecma-international.org/publications-and-standards/standards/ecma-262/)