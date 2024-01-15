---
title:                "Arbeiten mit yaml"
html_title:           "TypeScript: Arbeiten mit yaml"
simple_title:         "Arbeiten mit yaml"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/typescript/working-with-yaml.md"
---

{{< edit_this_page >}}

## Warum

YAML ist eine einfache und benutzerfreundliche Dateiformatierungssprache, die für die Darstellung von Daten in menschenlesbarer Form verwendet wird. Durch die Arbeit mit YAML können Entwickler komplexe Datenstrukturen übersichtlich und strukturiert darstellen, was die Arbeit mit großen Datensätzen deutlich erleichtert.

## Wie das geht

Hier sind einige einfache Beispiele dafür, wie man mit YAML in TypeScript arbeiten kann:

```TypeScript
const person = {
  name: "Max",
  age: 30,
  hobbies: ["Gaming", "Coding", "Reading"]
};
```

Wir können diese Datenstruktur nun in YAML-Format umwandeln:

```TypeScript
import YAML from "yaml";

const personYAML = YAML.stringify(person);

console.log(personYAML);
```

Dies gibt uns folgende Ausgabe:

```TypeScript
name: "Max"
age: 30
hobbies: 
- "Gaming"
- "Coding"
- "Reading"
```

Wir können auch YAML-Dateien einlesen und in JavaScript-Objekte umwandeln:

```TypeScript
const personYAML = `
  name: "Max"
  age: 30
  hobbies:
  - "Gaming"
  - "Coding"
  - "Reading"
`;

const person = YAML.parse(personYAML);

console.log(person.name);
//Output: "Max"
```

## Tieferer Einblick

YAML unterstützt eine Vielzahl von Datentypen, einschließlich Strings, Zahlen, Arrays und Objekte. Es gibt auch die Möglichkeit, benutzerdefinierte Datentypen und Verweise zu erstellen.

Zusätzlich zur einfachen Darstellung von Daten bietet YAML auch Funktionen wie Kommentare, die in der Datei angezeigt werden können, und die Einrückung, um die Lesbarkeit zu verbessern.

Eine besondere Stärke von YAML ist seine Integration mit anderen Programmiersprachen und Tools. YAML-Dokumente können problemlos in andere Dateiformate wie JSON oder XML umgewandelt werden.

## Siehe auch

- [offizielle YAML Spezifikation](http://yaml.org/spec/)
- [TypeScript YAML-Bibliothek](https://github.com/eemeli/yaml)
- [Tutorial zum Arbeiten mit YAML in Node.js](https://www.digitalocean.com/community/tutorials/an-introduction-to-yaml)