---
title:                "Arbeiten mit YAML"
html_title:           "TypeScript: Arbeiten mit YAML"
simple_title:         "Arbeiten mit YAML"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/typescript/working-with-yaml.md"
---

{{< edit_this_page >}}

## Was ist YAML und warum benutzen Programmierer es?

YAML steht für "YAML Ain't Markup Language" und ist eine einfache und übersichtliche Auszeichnungssprache, die häufig von Programmierern verwendet wird. Sie dient dazu, Datenstrukturen und Konfigurationsdateien zu erstellen, die leicht lesbar und verständlich sind. Viele Programmierer nutzen YAML, um ihre Arbeit zu vereinfachen und effizienter zu gestalten.

## Wie funktioniert es?

In TypeScript kann YAML über die Bibliothek "js-yaml" importiert werden. Beispielcode zeigt, wie man YAML-Dateien lesen und schreiben kann:

```TypeScript
import * as yaml from "js-yaml";

// YAML-Datei lesen
const data = yaml.load(fs.readFileSync("config.yaml", "utf-8"));

// YAML-Datei schreiben
const data = {
    title: "Mein Artikel",
    author: "Max Mustermann"
};
const yamlString = yaml.dump(data);
fs.writeFileSync("artikel.yaml", yamlString);
```

Die Ausgabe sieht dabei wie folgt aus:

```yaml
title: "Mein Artikel"
author: "Max Mustermann"
```

## Tiefergehende Einblicke

YAML wurde in den frühen 2000er Jahren entwickelt und basiert auf der Programmiersprache "Perl". Es ist eine Alternative zu XML und JSON und zeichnet sich durch seine einfache Syntax und Lesbarkeit aus. Außerdem gibt es auch Unterstützung für viele verschiedene Programmiersprachen wie JavaScript, Ruby und Python.

Manche Entwickler bevorzugen auch die Verwendung von JSON anstatt YAML, da es hier weniger Anpassungsmöglichkeiten gibt und somit die Struktur klarer definiert ist. Jedoch bietet YAML die Möglichkeit, Kommentare einzufügen und Daten übersichtlicher darzustellen.

Es ist wichtig zu beachten, dass YAML keine Markup-Sprache ist und somit nicht für die Darstellung von HTML-Code verwendet werden sollte. Es ist ausschließlich dafür gedacht, Datenstrukturen zu beschreiben.

## Weitere Informationen

Für weitere Informationen und Dokumentation zu YAML und dessen Verwendung in TypeScript können folgende Quellen genutzt werden:

- Offizielle YAML-Website: https://yaml.org/
- NPM-Paket für JavaScript/TypeScript: https://www.npmjs.com/package/js-yaml
- TypeScript-Dokumentation für den Umgang mit YAML: https://www.typescriptlang.org/docs/handbook/declaration-files/do-s-and-dont-s.html#library-declaration-d-ts-files