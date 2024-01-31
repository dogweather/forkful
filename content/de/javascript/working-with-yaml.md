---
title:                "Arbeiten mit YAML"
date:                  2024-01-19
html_title:           "Bash: Arbeiten mit YAML"
simple_title:         "Arbeiten mit YAML"

category:             "Javascript"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/javascript/working-with-yaml.md"
---

{{< edit_this_page >}}

## Was & Warum?

YAML ist ein Dateiformat für Konfigurationsdaten. Es ist einfach zu verstehen und wird oft in Projekten eingesetzt, um Einstellungen und Parameter zu definieren.

## Anleitung:

Um YAML in JavaScript zu nutzen, können wir die `js-yaml` Bibliothek verwenden. Zuerst installieren wir sie:

```bash
npm install js-yaml
```

Nun können wir YAML laden oder schreiben:

```javascript
const yaml = require('js-yaml');
const fs = require('fs');

// YAML Datei einlesen
try {
  const config = yaml.load(fs.readFileSync('./config.yaml', 'utf8'));
  console.log(config);
} catch (e) {
  console.error(e);
}

// JavaScript-Objekt als YAML speichern
const data = { title: 'Beispiel', count: 10 };
try {
  const yamlStr = yaml.dump(data);
  fs.writeFileSync('./output.yaml', yamlStr, 'utf8');
} catch (e) {
  console.error(e);
}
```

## Tiefere Einblicke:

YAML, was für „YAML Ain't Markup Language“ steht (ursprünglich hieß es „Yet Another Markup Language“), wurde Anfang der 2000er Jahre entwickelt. Es ist eine gute Alternative zu JSON und XML, da es menschenlesbarer ist. Achtung: YAML kann Sicherheitsrisiken bergen, wenn Inhalte unverifiziert geladen werden, denn es kann beliebigen Code enthalten. Deshalb immer Inhalte prüfen, bevor sie verarbeitet werden.

## Siehe Auch:

- YAML Spezifikation: https://yaml.org/spec.html
- `js-yaml` GitHub Repository: https://github.com/nodeca/js-yaml
- YAML vs. JSON: https://phoenixnap.com/kb/yaml-vs-json-vs-xml
