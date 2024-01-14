---
title:                "TypeScript: Arbeiten mit YAML"
simple_title:         "Arbeiten mit YAML"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/typescript/working-with-yaml.md"
---

{{< edit_this_page >}}

## Warum

YAML ist eine beliebte Dateiformatierung für die Speicherung von Daten in sehr strukturierter Form. Es bietet eine einfach zu lesende Syntax, die es Programmierern und Entwicklern ermöglicht, schnell und effizient Daten zu speichern und zu verarbeiten. YAML ist insbesondere in der TypeScript-Programmierung nützlich, da es die Definition komplexer Objekte mithilfe von einfachen Codeblöcken vereinfacht.

## How To

Um mit YAML in TypeScript zu arbeiten, muss zuerst das `js-yaml`-Paket installiert werden. Das kann einfach über den Befehl `npm install js-yaml` erfolgen. Nach der Installation kann YAML verwendet werden, indem das `js-yaml`-Paket importiert wird.

Im folgenden Beispiel wird gezeigt, wie ein Objekt in YAML-Format umgewandelt und wieder in ein TypeScript-Objekt konvertiert werden kann:

```TypeScript
import yaml from 'js-yaml';

// Beispiel-Objekt
const person = {
  name: 'Max Mustermann',
  alter: 30,
  adresse: {
    strasse: 'Musterstraße 1',
    stadt: 'Musterstadt',
    plz: 12345
  }
};

// YAML-Konvertierung des Objekts
const yamlData = yaml.dump(person);
console.log(yamlData);
// Ausgabe: name: 'Max Mustermann'
//         alter: 30
//         adresse:
//           strasse: 'Musterstraße 1'
//           stadt: 'Musterstadt'
//           plz: 12345

// Konvertierung von YAML zurück zu TypeScript
const personObj = yaml.load(yamlData);
console.log(personObj);
// Ausgabe: {
//            name: 'Max Mustermann',
//            alter: 30,
//            adresse: {
//              strasse: 'Musterstraße 1',
//              stadt: 'Musterstadt',
//              plz: 12345
//            }
//          }
```

## Deep Dive

YAML bietet neben der einfachen Speicherung von Objekten auch die Möglichkeit, Daten mit Tags zu versehen. Diese Tags können verwendet werden, um die Struktur der Daten zu definieren und somit die Lesbarkeit der Datei zu verbessern.

Ein weiteres nützliches Feature von YAML ist die Möglichkeit, Verweise auf andere Teile des Datenstroms zu erstellen. Das ermöglicht eine effizientere Verwaltung und Wiederverwendung von Daten.

Es ist auch wichtig zu beachten, dass YAML keine Beweis dafür ist, da es jedem Benutzer ermöglicht, die Datenstruktur beliebig zu erstellen und zu ändern. Daher ist es wichtig, sorgfältig zu überprüfen, ob die gelesenen Daten der erwarteten Struktur entsprechen, um unerwartete Fehler zu vermeiden.

## Siehe Auch

- [Offizielle YAML-Website] (http://yaml.org/)
- [NPM-Paket für js-yaml] (https://www.npmjs.com/package/js-yaml)
- [TypeScript-Dokumentation für YAML] (https://www.typescriptlang.org/docs/handbook/release-notes/typescript-2-3.html#example-enum-as-map-values)